(cl:in-package #:buoy-simulate)

;;; The most positive normal binary64 float has a mantissa with 53 1s
;;; (of which 52 are physically stored), encoding a number greater
;;; than or equal to 1, and less than 2, and an exponent of 1023
;;; (stored as 254).  We can get an integer with 53 1s by doing (1-
;;; (ash 1 53)) which we must then divide by (ash 1 52) to get a value
;;; slightly less than 2.  The result must then be multiplied by (ash
;;; 1 1023) to get the resulting floatr.
(defconstant most-positive-normal-binary64-floatr
  (* (1- (ash 1 53)) (ash 1 (- 1023 52))))

(defconstant least-positive-normal-binary64-floatr
  (/ (ash 1 1022)))

;;; The most positive submormal binary64 float has a mantissa of 52 1s
;;; encoding a number greater than or equal to 0 and less than 1, and
;;; and exponent of -1022 (stored as 0).  We can get an integer with 52
;;; 1s by doing (1- (ash 1 52)) which we must then divide by (ash 1
;;; 52) to get a value less than 1.  The result must then be divided
;;; by (ash 1 1022) to get the resulting floatr.
(defconstant most-positive-subnormal-binary64-floatr
  (/ (1- (ash 1 52)) (ash 1 (+ 1022 52))))

(defclass binary64 ()
  ())

(defclass binary64-normal-or-subnormal (binary64)
  ((%sign :initarg :sign :reader sign)
   (%value :initarg :value :reader value)))

(defclass binary64-normal (binary64-normal-or-subnormal)
  ())

(defclass binary64-subnormal (binary64-normal-or-subnormal)
  ())

(defclass binary64-infinity (binary64)
  ())

(defclass binary64-nan (binary64)
  ())

(defun binary64-from-rational (rational)
  (let ((exponent 0)
        (sign (if (minusp rational) -1 1))
        (mantissa (if (minusp rational) (- rational) rational)))
    (cond ((<= least-positive-normal-binary64-floatr
               rational
               most-positive-normal-binary64-floatr)
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
             ;; Now, we shift the numerator by 52 positions to get
             ;; something that should be an integer in the
             ;; floating-point representation.
             (setf numerator (ash numerator 52))
             (decf exponent 52)
             (let ((value (* sign
                             (if (minusp exponent)
                                 (/ (ash 1 (- exponent)))
                                 (ash 1 exponent))
                             (round (/ numerator denominator)))))
               (make-instance 'binary64-normal
                 :sign sign
                 :value value))))
          ((<= rational most-positive-subnormal-binary64-floatr)
           (make-instance 'binary64-subnormal
             :sign sign
             :value (/ (round (* rational (ash 1 (+ 52 1022))))
                       (ash 1 (+ 52 1022)))))
          ((< most-positive-subnormal-binary64-floatr
              rational
              least-positive-normal-binary64-floatr)
           (let* ((average (/ (+ least-positive-normal-binary64-floatr
                                 most-positive-subnormal-binary64-floatr)
                              2))
                  (value (if (< rational average)
                             most-positive-subnormal-binary64-floatr
                             least-positive-normal-binary64-floatr)))
             (make-instance 'binary64-subnormal
               :sign sign
               :value value))))))

(defun binary64-+ (x y)
  (binary64-from-rational (+ (value x) (value y))))

(defun binary64-- (x y)
  (binary64-from-rational (- (value x) (value y))))

(defun binary64-* (x y)
  (binary64-from-rational (* (value x) (value y))))

(defun binary64-/ (x y)
  (binary64-from-rational (/ (value x) (value y))))
