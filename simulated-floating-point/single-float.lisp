(cl:in-package #:buoy-simulate)

;;; Create a single-precision floating-point number represented as a
;;; non-negative integer.  SIGN is 0 (for positive) or 1 (for
;;; negative), and the corresponding bit will be stored in bit 31 of
;;; the result.  EXPONENT is taken to be an 8-bit non-negative
;;; integer, the bits of which will be stored in bits 30-23 of the
;;; result.  MANTISSA taken to be a 23-bit non-negative integer, the
;;; bits of which will be stored in the bits 22-0 of the result.
(defun single-float-from-components (sign exponent mantissa)
  (logior (ash sign 31) (ash exponent 23) mantissa))

(defun single-float-from-rational (rational)
  (let ((sign (if (minusp rational) 1 0))
        (mantissa (if (minusp rational) (- rational) rational))
        (exponent 0))
    (cond ((zerop mantissa)
           (single-float-from-components 0 0 0))
          ((>= mantissa (expt 2 128))
           (single-float-from-components sign 255 0))
          ((< mantissa (expt 2 -126))
           (single-float-from-components
            sign 0 (round (* mantissa (expt 2 (+ 23 126))))))
          (t
           (loop while (> mantissa (expt 2 23))
                 do (setf mantissa (/ mantissa 2))
                    (incf exponent))
           (loop while (< mantissa (expt 2 23))
                 do (setf mantissa (* mantissa 2))
                    (decf exponent))
           (single-float-from-components
            sign (+ exponent 127 23) (- (round mantissa) (expt 2 23)))))))

(defun rational-from-single-float (single-float)
  (let ((sign (ldb (byte 1 31) single-float))
        (exponent (ldb (byte 8 23) single-float))
        (mantissa (ldb (byte 23 0) single-float)))
    (when (= exponent 255)
      (error "can't convert infinity or NaN to rational"))
    (if (zerop exponent)
        (if (zerop mantissa)
            0
            (* sign mantissa (expt 2 (- (+ 23 126)))))
        (* sign
           (expt 2 (- exponent 127))
           (* (+ mantissa (expt 2 23)) (expt 2 -23))))))

(defconstant most-positive-single-float
  (single-float-from-components 0 254 (1- (expt 2 23))))

(defun single-float-binary-+ (single-float-1 single-float-2)
  (single-float-from-rational
   (+ (rational-from-single-float single-float-1)
      (rational-from-single-float single-float-2))))

(defun single-float-binary-- (single-float-1 single-float-2)
  (single-float-from-rational
   (- (rational-from-single-float single-float-1)
      (rational-from-single-float single-float-2))))

(defun single-float-binary-* (single-float-1 single-float-2)
  (single-float-from-rational
   (* (rational-from-single-float single-float-1)
      (rational-from-single-float single-float-2))))

(defun single-float-binary-/ (single-float-1 single-float-2)
  (single-float-from-rational
   (/ (rational-from-single-float single-float-1)
      (rational-from-single-float single-float-2))))
