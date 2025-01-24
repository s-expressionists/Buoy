(cl:in-package #:buoy-simulate)

;;; Create a double-precision floating-point number represented as a
;;; non-negative integer.  SIGN is 0 (for positive) or 1 (for
;;; negative), and the corresponding bit will be stored in bit 63 of
;;; the result.  EXPONENT is taken to be an 11-bit non-negative
;;; integer, the bits of which will be stored in bits 62-52 of the
;;; result.  MANTISSA taken to be a 52-bit non-negative integer, the
;;; bits of which will be stored in the bits 51-0 of the result.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun double-float-from-components (sign exponent mantissa)
    (logior (ash sign 63) (ash exponent 52) mantissa)))

(defun double-float-from-rational (rational)
  (let ((sign (if (minusp rational) 1 0))
        (mantissa (if (minusp rational) (- rational) rational))
        (exponent 0))
    (cond ((zerop mantissa)
           (double-float-from-components 0 0 0))
          ((>= mantissa (expt 2 1024))
           (double-float-from-components sign 2047 0))
          ((< mantissa (expt 2 -1022))
           (double-float-from-components
            sign 0 (round (* mantissa (expt 2 (+ 52 1022))))))
          (t
           (loop while (> mantissa (expt 2 52))
                 do (setf mantissa (/ mantissa 2))
                    (incf exponent))
           (loop while (< mantissa (expt 2 52))
                 do (setf mantissa (* mantissa 2))
                    (decf exponent))
           (double-float-from-components
            sign (+ exponent 127 52) (- (round mantissa) (expt 2 52)))))))

(defconstant most-positive-double-float
  (double-float-from-components 0 2046 (1- (expt 2 52))))

(defconstant most-negative-double-float
  (double-float-from-components 1 2046 (1- (expt 2 52))))

(defconstant least-positive-double-float
  (double-float-from-components 0 0 1))

(defconstant least-positive-normalized-double-float
  (double-float-from-components 0 1 0))

(defconstant least-negative-double-float
  (double-float-from-components 1 0 1))

(defconstant least-negative-normalized-double-float
  (double-float-from-components 1 1 0))

(defun integer-decode-double-float-normalized (double-float)
  (values (+ (ldb (byte 52 0) double-float) (expt 2 52))
          (- (ldb (byte 11 52) double-float) 1023 52)
          (if (logbitp 63 double-float) -1 1)))

(defun integer-decode-double-float-denormalized (double-float)
  (values (ldb (byte 52 0) double-float)
          (- (+ 1022 52))
          (if (logbitp 63 double-float) -1 1)))

(defun integer-decode-double-float (double-float)
  (let ((exponent (ldb (byte 11 52) double-float))
        (mantissa (ldb (byte 52 0) double-float)))
    (cond ((= exponent 2047)
           (if (zerop mantissa)
               (error "Can't decode infinity")
               (error "Can't decode NaN")))
          ((zerop exponent)
           (if (zerop mantissa)
               (values 0 0 1)
               (integer-decode-double-float-denormalized double-float)))
          (t
           (integer-decode-double-float-normalized double-float)))))

(defun rational-from-double-float (double-float)
  (multiple-value-bind (mantissa exponent sign)
      (integer-decode-double-float double-float)
    (* sign mantissa (expt 2 exponent))))

(defun double-float-binary-+ (double-float-1 double-float-2)
  (double-float-from-rational
   (+ (rational-from-double-float double-float-1)
      (rational-from-double-float double-float-2))))

(defun double-float-binary-- (double-float-1 double-float-2)
  (double-float-from-rational
   (- (rational-from-double-float double-float-1)
      (rational-from-double-float double-float-2))))

(defun double-float-binary-* (double-float-1 double-float-2)
  (double-float-from-rational
   (* (rational-from-double-float double-float-1)
      (rational-from-double-float double-float-2))))

(defun double-float-binary-/ (double-float-1 double-float-2)
  (double-float-from-rational
   (/ (rational-from-double-float double-float-1)
      (rational-from-double-float double-float-2))))
