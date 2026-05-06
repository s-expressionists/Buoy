(cl:in-package #:buoy-simulate)

;;; This code is reasonably fast.  Calling RATIONAL-EXP with an
;;; argument of 700 (which is close to the max for a double float)
;;; takes 0.05 seconds.  It gives a precision of at least 60 bits.  We
;;; could improve the precision by dividing by something more than (1+
;;; diff).  And we can speed it up by limiting the precision to (say)
;;; 256 bits in each iteration.
;;;
;;; We could also pre-compute 1/n! and turn each value into a floatr,
;;; as well as turning the argument into a floatr.  That way, each
;;; term in the Taylor series would be a multiplication of floatrs,
;;; which is a multiplication of the nominators and an additon of the
;;; denominators.  And after adding a new term, we could turn the
;;; result into a floatr.

(defun power (x integer)
  (cond ((zerop integer) 1)
        ((evenp integer)
         (power (* x x) (/ integer 2)))
        (t
         (* x (power x (1- integer))))))

(defun rational-exp-with-small-argument (argument)
  (1+ (loop for numerator = argument then (* numerator argument)
            for i from 1 to 20
            for denominator = 1 then (* denominator i)
            sum (/ numerator denominator))))

(defun rational-exp (argument)
  (if (> argument 1)
      (let* ((numerator (numerator argument))
             (numerator-length (integer-length numerator))
             (denominator (denominator argument))
             (denominator-length (integer-length denominator))
             (diff (- numerator-length denominator-length)))
        (power (rational-exp-with-small-argument
                (/ numerator (ash denominator (1+ diff))))
               (ash 1 (1+ diff))))
      (rational-exp-with-small-argument argument)))
