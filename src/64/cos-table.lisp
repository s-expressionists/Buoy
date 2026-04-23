(cl:in-package #:buoy-core-math-64)

(defun very-slow-cos-rational (x)
  (1+ (loop for i from 2 to 200 by 2
            for sign = -1 then (- sign)
            for factorial = 2 then (* factorial (1- i) i)
            for denominator = (* x x)  then (* denominator x x)
            for term = (* sign (/ denominator factorial))
            for result = term then (+ result term)
            until (< (/ (abs term) (1+ result)) #.(/ (ash 1 150)))
            finally (return result))))

;;; This table cntains approximations of (cos (/ (* 2 pi i) (expt 2
;;; 11)) for (<= 0 i 255).  For some reason, the core-math library
;;; does not nterpret the entries of this table the same way as it
;;; interpret other custom-float-64 numbers.  The difference is a
;;; factor 2 which is why we multiplie by 2 before converting to a
;;; custom-float-64.
(defparameter *cos-table*
  (let ((result (make-array 256)))
    (loop with multplier = (/ (* 2 *less-precise-pi-rational*) (expt 2 11))
          for i from 0 below 256
          for cos-rational = (very-slow-cos-rational (* multplier i))
          do (setf (aref result i)
                   (custom-float-64-from-rational cos-rational))
          finally (return result))))
