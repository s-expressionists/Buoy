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

(defparameter *inverses*
  (loop for i from 1 to 100 collect (pf:pfloat-from-rational (/ i))))

(defparameter *pfloat-exp-limit*
  (pf:pfloat-from-rational (expt 2 -200)))

(defun pfloat-exp-with-small-positive-argument (rational)
  (loop with pfloat-argument = (pf:pfloat-from-rational rational)
        for term = pf:*one* then (pf:* (pf:* term pfloat-argument) inv)
        for sum = pf:*one* then (pf:+ sum term)
        for inv in *inverses*
        do (when (pf:< term *pfloat-exp-limit*)
             (loop-finish))
        finally (return sum)))

;;; Compute PFLOAT to the power of the second argument.  The second
;;; argument is an integer that is alwo a power of 2.
(defun pfloat-power (pfloat power-of-two)
  (loop with result = pfloat
        until (= power-of-two 1)
        do (setf result (pf:* result result))
           (setf power-of-two (ash power-of-two -1))
        finally (return result)))
  
(defun pfloat-exp-with-positive-argument (pfloat)
  (let ((magnitude (+ (pf:exponent pfloat) pf:*precision*))
        (copy (pf:make-pfloat (pf:mantissa pfloat)
                              (pf:exponent pfloat)))
        (pfloat-power 1))
    ;; When the magnitute is positive, the argument is larger than
    ;; what we would like, and in fact, we want it to be a bit smaller
    ;; than required to make the magnitude negative. 
    (when (> magnitude -2)
      (setf pfloat-power (ash 1 (1+ (integer-length (- magnitude 2)))))
      (decf (pf:exponent copy) magnitude))
    (let ((exp (pfloat-exp-with-small-positive-argument copy)))
      (pfloat-power exp pfloat-power))))

(defun pfloat-exp (pfloat)
  (if (pf:minusp pfloat)
      (pf:/ pf:*one* (pfloat-exp-with-positive-argument (pf:negate pfloat)))
      (pfloat-exp-with-positive-argument pfloat)))
