(cl:in-package #:buoy-simulate)

;;; The algorithm below works best for arguments that are slightly
;;; larger than 1.  Otherwise, convergence is not great.  To make sure
;;; we have an argument that is between 1 (inclusive) and 2
;;; (exclusive), we use the equivalence between (LOG (* (EXPT 2 N) X))
;;; and (+ (* N (LOG 2)) (LOG X)).  Furthermore, when X is close to 2
;;; (so that the Y above is close to 2), then convergence of the
;;; algorithm is not so good.  To fix that, we use the equivalence
;;; between (LOG (EXPT X 2)) and (* 2 (LOG X)).  We can then take the
;;; square root of the argument until it is small enough that
;;; convergence is acceptable.

;;; FIXME: Do this better.
(defparameter *ln-iteration-count* 20)

(defun rational-ln-with-small-argument (argument count)
  (let* ((quotient (/ (1- argument) (1+ argument)))
         (square (* quotient quotient)))
    (* 2 (loop for numerator = quotient then (* numerator square)
               for denominator from 1 to count by 2
               sum (/ numerator denominator)))))

(defparameter *ln-2*
  (* 2 (rational-ln-with-small-argument (rational-square-root 2) 20)))
