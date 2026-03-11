(cl:in-package #:buoy-simulate)

(defun rational-ln-with-small-argument (argument)
  (let* ((quotient (/ (1- argument) (1+ argument)))
         (square (* quotient quotient)))
    (* 2 (loop for numerator = quotient then (* numerator square)
               for denominator from 1 to 20 by 2
               sum (/ numerator denominator)))))
