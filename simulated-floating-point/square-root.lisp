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
