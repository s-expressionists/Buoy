(cl:in-package #:buoy-core-math-64)

;;; This code creates an entry that fulfills the criteria in log.c,
;;; but it does not generate the same entry for values of i 369, 387,
;;; and 397.
(defun generate-entry (i)
  (let* ((low (* i (expt 2 -9)))
         (high (* (1+ i) (expt 2 -9)))
         (low-inverse (/ high))
         (high-inverse (/ low))
         (average (buoy-simulate:rational-square-root (* low-inverse high-inverse)))
         (floatr (buoy-simulate:floatr-from-rational average 10 9))
         (y-low (* i (expt 2 -9)))
         (y-high (* (1+ i) (expt 2 -9))))
    (assert (<= (dfloat (abs (1- (* floatr y-low)))) 0.00212097167968735d0))
    (assert (<= (dfloat (abs (1- (* floatr y-high)))) 0.00212097167968735d0))
    (dfloat floatr)))
         
;;; The following is the comment in log.c, but it doesn't make sense
;;; as parsed.
;;;
;;; For 362 <= i <= 724, r[i] = _INVERSE[i-362] is a 10-bit
;;; approximation of 1/x[i], where i*2^-9 <= x[i] < (i+1)*2^-9. More
;;; precisely r[i] is a 10-bit value such that r[i]*y-1 is
;;; representable exactly on 53 bits for any y, i*2^-9 <= y <
;;; (i+1)*2^-9. Moreover |r[i]*y-1| <= 0.00212097167968735. */

(defparameter *inverse-table*
  (make-array
   363
   :element-type 'double-float
   :initial-contents
   (loop for i from 362 to 724
         for index from 0
         collect (generate-entry i))))
