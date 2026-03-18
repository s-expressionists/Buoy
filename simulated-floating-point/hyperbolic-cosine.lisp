(cl:in-package #:buoy-simulate)

;;; I haven't found a way to reduce the argument, so convergence is
;;; going to be slow for large values of the argument.

(defun rational-hyperbolic-cosine (argument)
  (loop for i from 0 by 2
        for numerator = 1 then (* numerator argument argument)
        for denominator = 1 then (* denominator (1- i) i)
        for quotient = (/ numerator denominator)
        until (< quotient #.(/ (ash 2 120)))
        sum quotient))
