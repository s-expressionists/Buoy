(cl:in-package #:buoy-core-math-64)

;;; The exact sum of HIGH and LOW is the exact sum of A and B.
(defun fast-two-sum (a b)
  (declare (type double-float a b))
  (let* ((high (+ a b))
         (err (- high a)) ; exact
         (low (- b err))) ; exact
    (values high low)))

;;; The core-math library uses a C99 function called `fma'.  It takes
;;; three arguments, x, y, and z and it returns (+ (* x y) z),
;;; apparently without losing any intermediate precision.  The only
;;; way I can think of implementing this in standard Common Lisp is to
;;; use rationals, but that won't be fast.  I also don't know how C99
;;; compilers generate code for it, unless the architecture has an
;;; instruction that does that.
(defun fma (x y z)
  (declare (type double-float x y z))
  (float (+ (* (rational x) (rational y))
            (rational z))
         1d0))
