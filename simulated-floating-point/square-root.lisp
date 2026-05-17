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
(defun newton-iteration-count (precision)
  (loop with argument = 4
        for approximation = argument
          then (newton-iteration argument approximation)
        count t
        until (< (/ (* approximation approximation) argument)
                 (+ 1 precision))))

;;; Determine the iteration count if we want to compute the sqare root
;;; with a precision of (EXPT 2 -120) which is a bit more than the
;;; precision of quadruple-precision IEEE floating point.
(defparameter *newton-iteration-count*
  (newton-iteration-count (/ (ash 1 120))))

;;; We use the equivalence between (sqrt x) and (* (expt 2 (- n))
;;; (sqrt (* x (expt 2 (* 2 n))))) to scale the argument so that it is
;;; between 1 (inclusive) and 4 (exclusive).  That way, we are sure
;;; that *NEWTON-ITERATION-COUNT* is enough to get a result with the
;;; desired precision.
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
    ;; magnitude. So we compute the square root of it.
    (let* ((result (newton (/ numerator denominator)
                           *newton-iteration-count*))
           (result-numerator (numerator result))
           (result-denominator (denominator result)))
      (if (minusp diff)
          (/ result-numerator (ash result-denominator (- (/ diff 2))))
          (/ (ash result-numerator (/ diff 2)) result-denominator)))))

(defparameter *rational-square-root-of-2*
  (loop for root = 2 then (/ (+ root (/ 2 root)) 2)
        repeat 10
        finally (return root)))

(defparameter *pfloat-square-root-of-2*
  (pf:pfloat-from-rational *rational-square-root-of-2*))

;;; The idea here is that we are going to use Newton's method on
;;; numbers between 0.5 and 2, and we are going to use the argument
;;; itself as the initial iteration value.  So the worst case is then
;;; when either 0.5 or 2 is the argument since that's when the
;;; argument and its root have the largest difference.  So we use the
;;; value 2 to compute the iteration count.
(defparameter *square-root-iteration-count*
  (loop with two = pf:*two*
        for root = two
          then (pf:/ (pf:+ root (pf:/ two root)) two)
        for i from 0
        until (equal root *pfloat-square-root-of-2*)
        finally (return i)))

(defun pfloat-square-root (argument)
  ;; We want to use the Newton iterations on an argument that is
  ;; between 0.5 and 2. So then the exponent must be either (-
  ;; pf:*precision*) and (- (1- pf:*precision*)).  We choose one or the
  ;; other so that the difference between the current exponent and the
  ;; desired exponent is even.
  (let* ((exponent (pf:exponent argument))
         (diff (if (evenp (- exponent pf:*precision*))
                   (- exponent (- pf:*precision*))
                   (- exponent (- (1- pf:*precision*)))))
         (reduced (pf:make-pfloat (pf:mantissa argument)
                                  (- exponent diff)))
         (newton
           (loop for root = reduced
                   then (pf:/ (pf:+ root (pf:/ reduced root)) pf:*two*)
                 repeat *square-root-iteration-count*
                 finally (return root))))
    (pf:make-pfloat (pf:mantissa newton)
                    (+ (pf:exponent newton) (/ diff 2)))))
    
