(cl:in-package #:buoy-simulate)

;;; The coefficients in the Taylor expansion of ARC SINE are too
;;; messed up to simplify.  It is better to just write a function to
;;; compute each coefficient.

(defun factorial (n)
  (if (zerop n)
      1
      (loop for i from 1 to n
            for result = 1 then (* result i)
            finally (return result))))

(defun arc-sine-term-coefficient (n)
  (flet ((square (x) (* x x)))
    (/ (factorial (* 2 n)) (* (expt 4 n) (square (factorial n)) (1+ (* 2 n))))))

(defparameter *arc-sine-factors*
  (cons pf:*one*
        (loop for i from 1 to 100
              collect (pf:pfloat-from-rational
                       (/ (arc-sine-term-coefficient i)
                          (arc-sine-term-coefficient (1- i)))))))

(defun pfloat-arc-sine (pfloat)
  (loop with sum = pf:*zero*
        with square = (pf:* pfloat pfloat)
        for factor in *arc-sine-factors*
        for term = pfloat then (pf:* (pf:* term square) factor)
        for sum2 = term then  (pf:+ sum term)
        until (pf:= sum sum2)
        do (setf sum sum2)
        finally (return sum)))

(defun rational-arc-sine (rational)
  (let* ((pfloat (pf:pfloat-from-rational rational))
         (pfloat-result (pfloat-arc-sine pfloat)))
    (pf:rational-from-pfloat pfloat-result)))
