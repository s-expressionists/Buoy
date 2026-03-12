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
