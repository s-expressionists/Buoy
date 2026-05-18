(cl:in-package #:buoy-simulate)

(defparameter *arcsine-iteration-count* 20)

(defun rational-arcsine (argument)
  (loop for n from 0 below *arcsine-iteration-count*
        for 2n! = 1 then (* 2n! (1- (* 2 n)) (* 2 n))
        for 4-to-the-n = 1 then (* 4-to-the-n 4)
        for n! = 1 then (* n! n)
        for n!-squared = (* n! n!)
        for 2n+1 = 1 then (+ 2n+1 n n)
        for term = argument then (* term argument argument)
        sum (/ (* 2n! term)
               (* 4-to-the-n n!-squared 2n+1))))

;;; The coefficients in the Taylor expansion of ARCSINE are too messed
;;; up to simplify.  It is better to just write a function to compute
;;; each coefficient.

(defun factorial (n)
  (if (zerop n)
      1
      (loop for i from 1 to n
            for result = 1 then (* result i)
            finally (return result))))

(defun arcsine-term-coefficient (n)
  (flet ((square (x) (* x x)))
    (/ (factorial (* 2 n)) (* (expt 4 n) (square (factorial n)) (1+ (* 2 n))))))

(defparameter *arcsine-factors*
  (cons pf:*one*
        (loop for i from 1 to 100
              collect (pf:pfloat-from-rational
                       (/ (arcsine-term-coefficient i)
                          (arcsine-term-coefficient (1- i)))))))

(defun pfloat-arcsine (pfloat)
  (loop with sum = pf:*zero*
        with square = (pf:* pfloat pfloat)
        for factor in *arcsine-factors*
        for term = pfloat then (pf:* (pf:* term square) factor)
        for sum2 = term then  (pf:+ sum term)
        until (pf:= sum sum2)
        do (setf sum sum2)
        finally (return sum)))

