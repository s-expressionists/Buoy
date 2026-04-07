(cl:in-package #:buoy-core-math-64)

;;; To compute the sine table, we can do with a less precise rational
;;; pi.  We have checked that the computation of the sine table gives
;;; the same result as that of the core-math library with this value
;;; of rational pi.
(defparameter *less-precise-pi-rational*
  (let ((pi-string "3141592653589793238462643383279502884197169"))
    (/ (read-from-string pi-string)
       (expt 10 (1- (length pi-string))))))

(defun very-slow-sin-rational (x)
  (loop for i from 1 by 2
        for sign = 1 then (- sign)
        for factorial = 1 then (* factorial (1- i) i)
        for denominator = x then (* denominator x x)
        for term = (* sign (/ denominator factorial))
        for result = term then (+ result term)
        until (< (/ (abs term) (1+ result)) #.(/ (ash 1 150)))
        finally (return result)))

;;; This table cntains approximations of (sin (/ (* 2 pi i) (expt 2
;;; 11)) for (<= 0 i 255).  For some reason, the core-math library
;;; does not nterpret the entries of this table the same way as it
;;; interpret other custom-float-64 numbers.  The difference is a
;;; factor 2 which is why we multiplie by 2 before converting to a
;;; custom-float-64.
(defparameter *sin-table*
  (let ((result (make-array 256)))
    (setf (aref result 0)
          (make-custom-float-64 :high 0 :low 0 :exponent 128 :sign 0))
    (loop with multplier = (/ (* 2 *less-precise-pi-rational*) (expt 2 11))
          for i from 1 below 256
          for sin-rational = (very-slow-sin-rational (* multplier i))
          do (setf (aref result i)
                   (custom-float-64-from-rational (* 2 sin-rational)))
          finally (return result))))

