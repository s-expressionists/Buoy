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

(defparameter *factors*
  (cons pf:*zero*
        (loop for i from 3 by 2 to 100
              collect (pf:pfloat-from-rational (/ (- i 2) i)))))

;;; This function should be called when the argument is (1+ X) where X
;;; is a small positive value, preferably less than 0.1.  It uses the
;;; series 2/(2k+1) * (a-1)/(a+1)^(2k+1) with k from 0.
(defun pfloat-ln-with-small-argument (argument)
  (let* ((quotient (pf:/ (pf:- argument pf:*one*)
                         (pf:+ argument pf:*one*)))
         (square (pf:* quotient quotient)))
    (loop with sum = pf:*zero*
          for k from 1 by 2
          for factor in *factors*
          for term = (pf:* quotient pf:*two*)
            then (pf:* (pf:* term square) factor)
          for sum2 = term then (pf:+ sum term)
          until (pf:= sum sum2)
          do (setf sum sum2)
          finally (return sum))))

(defparameter *pfloat-ln-2*
  (let ((reduced *pfloat-square-root-of-2*))
    (pf:* pf:*two* (pfloat-ln-with-small-argument reduced))))

(defun pfloat-ln (argument)
  (let* ((exponent (pf:exponent argument))
         (desired-exponent (- (1- pf:*precision*)))
         (diff (- exponent desired-exponent))
         (reduced (pf:make-pfloat (pf:mantissa argument) desired-exponent))
         (reduced (pfloat-square-root reduced))
         (reduced (pfloat-square-root reduced)))
    (pf:+ (pf:* pf:*two*
                (pf:* pf:*two*
                      (pfloat-ln-with-small-argument reduced)))
          (pf:* (pf:pfloat-from-rational diff) *pfloat-ln-2*))))
