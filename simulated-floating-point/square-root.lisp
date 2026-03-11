(cl:in-package #:buoy-simulate)

;;; Compute a single iteration with the Newton-Raphson method for
;;; computing the square root of ARGUMENT.
(defun newton-iteration (argument approximation)
  (/ (+ approximation (/ argument approximation)) 2))

;;; Compute the square root of ARGUMENT using COUNT iterations.
(defun newton (argument count)
  (loop for approximation = argument
          then (newton-iteration argument approximation)
        repeat count
        finally (return approximation)))

;;; We are going to take the square root of arguments between 1
;;; (inclusive) and 4 (exclusive), and we are going to give the
;;; argument itself as the initial appoximation.  This function
;;; computes how many iteration steps we need in order to compute the
;;; result with a certain precision.  PRECISION is a small fraction.
;;; Since the worst case is when something close to 4 is given, this
;;; is the value we use to compute the iteration count.
(defun iteration-count (precision)
  (loop with argument = 4
        for approximation = argument
          then (newton-iteration argument approximation)
        count t
        until (< (/ (* approximation approximation) argument)
                 (+ 1 precision))))

;;; Determine the iteration count if we want to compute the sqare root
;;; with a precision of (EXPT 2 -120) which is a bit more than the
;;; precision of quadruple-precision IEEE floating point.
(defparameter *iteration-count*
  (iteration-count (/ (ash 1 120))))

;;; We use the equivalence between (sqrt x) and
;;; (* (expt 2 (- n)) (sqrt (* x (expt 2 (* 2 n))))) to scale the 
;;; argument so that it is between 1 (inclusive) and 4 (exclusive).
;;; That way, we are sure that *ITERATION-COUNT* is enough to get
;;; a result with the desired precision.
(defun rational-square-root (rational)
  (let* ((numerator (numerator rational))
         (numerator-length (integer-length numerator))
         (denominator (denominator rational))
         (denominator-length (integer-length denominator))
         (diff (- numerator-length denominator-length)))
    (if (minusp diff)
        ;; We need to increase the numerator.
        (setf numerator (ash numerator (- diff)))
        ;; We need to increase the denominator.
        (setf denominator (ash denominator diff)))
    (if (< numerator denominator)
        ;; We need to shift the numerator by 1 or 2 steps so that DIFF
        ;; is even.
        (if (evenp diff)
            (progn (setf numerator (ash numerator 2))
                   (decf diff 2))
            (progn (setf numerator (ash numerator 1))
                   (decf diff 1)))
        ;; Numerator is greater than or equal to the denominator, but
        ;; it is possible that DIFF is odd, and in that case we need
        ;; to shift the numerator by one step.
        (when (oddp diff)
          (setf numerator (ash numerator 1))
          (decf diff 1)))
    ;; Now (/ numerator denominator) is a number of the right
    ;; magnitude. So we copute the square root of it.
    (let* ((result (newton (/ numerator denominator) *iteration-count*))
           (result-numerator (numerator result))
           (result-denominator (denominator result)))
      (if (minusp diff)
          (/ result-numerator (ash result-denominator (- (/ diff 2))))
          (/ (ash result-numerator (/ diff 2)) result-denominator)))))
