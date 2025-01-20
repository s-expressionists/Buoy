(cl:in-package #:buoy-simulate)

;;; Create a double-precision floating-point number represented as a
;;; non-negative integer.  SIGN is 0 (for positive) or 1 (for
;;; negative), and the corresponding bit will be stored in bit 63 of
;;; the result.  EXPONENT is taken to be an 11-bit non-negative
;;; integer, the bits of which will be stored in bits 62-52 of the
;;; result.  MANTISSA taken to be a 52-bit non-negative integer, the
;;; bits of which will be stored in the bits 51-0 of the result.
(defun double-float-from-components (sign exponent mantissa)
  (logior (ash sign 63) (ash exponent 52) mantissa))

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

(defun rational-from-double-float (double-float)
  (let ((sign (ldb (byte 1 63) double-float))
        (exponent (ldb (byte 11 52) double-float))
        (mantissa (ldb (byte 52 0) double-float)))
    (when (= exponent 2047)
      (error "can't convert infinity or NaN to rational"))
    (if (zerop exponent)
        (if (zerop mantissa)
            0
            (* sign mantissa (expt 2 (- (+ 52 1022)))))
        (* sign
           (expt 2 (- exponent 127))
           (* (+ mantissa (expt 2 52)) (expt 2 -52))))))

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
