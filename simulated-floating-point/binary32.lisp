(cl:in-package #:buoy-simulate)

;;; The most positive normal binary32 float has a mantissa with 24 1s
;;; (of which 23 are physically stored), encoding a number greater
;;; than or equal to 1, and less than 2, and an exponent of 127
;;; (stored as 254).  We can get an integer with 24 1s by doing (1-
;;; (ash 1 24)) which we must then divide by (ash 1 23) to get a value
;;; slightly less than 2.  The result must then be multiplied by (ash
;;; 1 127) to get the resulting floatr.
(defconstant most-positive-normal-binary32-floatr
  (* (1- (ash 1 24)) (ash 1 (- 127 23))))

(defconstant least-positive-normal-binary32-floatr
  (/ (ash 1 126)))

;;; The most positive submormal binary32 float has a mantissa of 23 1s
;;; encoding a number greater than or equal to 0 and less than 1, and
;;; and exponent of -126 (stored as 0).  We can get an integer with 23
;;; 1s by doing (1- (ash 1 23)) which we must then divide by (ash 1
;;; 23) to get a value less than 1.  The result must then be divided
;;; by (ash 1 126) to get the resulting floatr.
(defconstant most-positive-subnormal-binary32-floatr
  (/ (1- (ash 1 23)) (ash 1 (+ 126 23))))

(defclass binary32 ()
  ())

(defclass binary32-normal-or-subnormal (binary32)
  ((%sign :initarg :sign :reader sign)
   (%value :initarg :value :reader value)))

(defclass binary32-normal (binary32-normal-or-subnormal)
  ())

(defclass binary32-subnormal (binary32-normal-or-subnormal)
  ())

(defclass binary32-infinity (binary32)
  ())

(defclass binary32-nan (binary32)
  ())

(defun binary32-from-rational (rational)
  (let ((exponent 0)
        (sign (if (minusp rational) -1 1))
        (mantissa (if (minusp rational) (- rational) rational)))
    (cond ((<= least-positive-normal-binary32-floatr
               rational
               most-positive-normal-binary32-floatr)
           (let* ((numerator (numerator mantissa))
                  (numerator-length (integer-length numerator))
                  (denominator (denominator mantissa))
                  (denominator-length (integer-length denominator))
                  (difference (- numerator-length denominator-length)))
             (setf exponent difference)
             (if (minusp difference)
                 ;; Then we shift the numerator by the negative
                 ;; difference.
                 (setf numerator (ash numerator (- difference)))
                 ;; Otherwise, we shift the denominator by the
                 ;; difference.
                 (setf denominator (ash denominator difference)))
             ;; At this point either the numerator is less than the
             ;; denominator so that the quotient is less than 1, or
             ;; the numerator is greater than or equal to the
             ;; denominator, so that the quotient is greater than or
             ;; equal to 1.  In the second case, we have something
             ;; that can be used as the mantissa of the float but in
             ;; the first case, we must multiply the denominator by 2.
             (when (< numerator denominator)
               (decf exponent)
               (setf numerator (ash numerator 1)))
             ;; Now, we shift the numerator by 23 positions to get
             ;; something that should be an integer in the
             ;; floating-point representation.
             (setf numerator (ash numerator 23))
             (decf exponent 23)
             (let ((value (* sign
                             (if (minusp exponent)
                                 (/ (ash 1 (- exponent)))
                                 (ash 1 exponent))
                             (round (/ numerator denominator)))))
               (make-instance 'binary32-normal
                 :sign sign
                 :value value))))
          ((<= rational most-positive-subnormal-binary32-floatr)
           (make-instance 'binary32-subnormal
             :sign sign
             :value (/ (round (* rational (ash 1 (+ 23 126))))
                       (ash 1 (+ 23 126)))))
          ((< most-positive-subnormal-binary32-floatr
              rational
              least-positive-normal-binary32-floatr)
           (let* ((average (/ (+ least-positive-normal-binary32-floatr
                                 most-positive-subnormal-binary32-floatr)
                              2))
                  (value (if (< rational average)
                             most-positive-subnormal-binary32-floatr
                             least-positive-normal-binary32-floatr)))
             (make-instance 'binary32-subnormal
               :sign sign
               :value value))))))

(defun binary32-+ (x y)
  (binary32-from-rational (+ (value x) (value y))))

(defun binary32-- (x y)
  (binary32-from-rational (- (value x) (value y))))

(defun binary32-* (x y)
  (binary32-from-rational (* (value x) (value y))))

(defun binary32-/ (x y)
  (binary32-from-rational (/ (value x) (value y))))
