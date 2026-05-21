(cl:in-package #:buoy-simulate)

(defparameter *inverses*
  (loop for i from 1 to 100 collect (pf:pfloat-from-rational (/ i))))

(defparameter *pfloat-exp-limit*
  (pf:pfloat-from-rational (expt 2 -200)))

(defun pfloat-exp-with-small-positive-argument (pfloat)
  (loop for term = pf:*one* then (pf:* (pf:* term pfloat) inv)
        for sum = pf:*one* then (pf:+ sum term)
        for inv in *inverses*
        do (when (pf:< term *pfloat-exp-limit*)
             (loop-finish))
        finally (return sum)))

;;; Compute PFLOAT to the power of the second argument.  The second
;;; argument is an integer that is alwo a power of 2.
(defun pfloat-power (pfloat n)
  (loop with result = pfloat
        repeat n
        do (setf result (pf:* result result))
        finally (return result)))
  
(defun pfloat-exp-with-positive-argument (pfloat)
  (let ((magnitude (+ (pf:exponent pfloat) pf:*precision*))
        (reduced pfloat)
        (reduction 0))
    ;; When the magnitute is positive, the argument is larger than
    ;; what we would like, and in fact, we want it to be a bit smaller
    ;; than required to make the magnitude negative. 
    (when (> magnitude -2)
      (setf reduction (+ magnitude 2))
      (setf reduced
            (pf:make-pfloat (pf:mantissa pfloat)
                            (- (pf:exponent pfloat) reduction))))
    (let ((exp (pfloat-exp-with-small-positive-argument reduced)))
      (pfloat-power exp reduction))))

(defun pfloat-exp (pfloat)
  (if (pf:minusp pfloat)
      (pf:/ pf:*one* (pfloat-exp-with-positive-argument (pf:negate pfloat)))
      (pfloat-exp-with-positive-argument pfloat)))

(defun rational-exp (rational)
  (let* ((pfloat (pf:pfloat-from-rational rational))
         (pfloat-exp (pfloat-exp pfloat)))
    (pf:rational-from-pfloat pfloat-exp)))

(defun double-float-exp (double-float)
  (let* ((pfloat (pf:pfloat-from-rational (rational double-float)))
         (pfloat-exp (pfloat-exp pfloat))
         (rational-result (pf:rational-from-pfloat pfloat-exp)))
    (dfloat rational-result)))

(defun pfloat-expt (base exponent)
  (pfloat-exp (pf:* exponent (pfloat-ln base))))

(defun rational-expt (base exponent)
  (let ((pfloat-base (pf:pfloat-from-rational base))
        (pfloat-exponent (pf:pfloat-from-rational exponent)))
    (pf:rational-from-pfloat (pfloat-expt pfloat-base pfloat-exponent))))

        
        
