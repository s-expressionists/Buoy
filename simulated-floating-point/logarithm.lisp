(cl:in-package #:buoy-simulate)

;;; The Taylor series for (LOG (1+ Y) where Y is a positive number
;;; less than 1 is the sum of the infinite series of terms (/ (EXPT Y
;;; K) K) with alternating signs.  To make sure we have an argument
;;; that is between 1 (inclusive) and 2 (exclusive), we use the
;;; equivalence between (LOG (* (EXPT 2 N) X)) and (+ (* N (LOG 2))
;;; (LOG X)).  Furthermore, when X is close to 2 (so that the Y above
;;; is close to 2), then convergence of the Taylor series is very
;;; slow.  To fix that, we use the equivalence between (LOG (EXPT X
;;; 2)) and (* 2 (LOG X)).  We can then take the square root of the
;;; argument until it is small enough that convergence is acceptable.
;;; So, for instance, if X is less than 1.03 or (1+ (/ (ash 1 5))), we
;;; gain at least 5 binary digits in each iteration.  With a desired
;;; precision of 112 binary digits (as in quadruple-precision
;;; floating-point, we need 20 or so iterations.

(defun rational-ln-with-small-argument (argument)
  (loop with epsilon = (1- argument)
        for x = epsilon then (* x epsilon)
        for y from 1 to 20
        sum (/ x y)))
