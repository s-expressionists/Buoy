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

;;; Multiply exactly A and B such that the sum of HIGH and LOW is the
;;; exact product of A and B.  The core-math library call this
;;; function `a_mul' and I have no ideas for a much improved name.
(defun a-multiply (a b)
  (declare (type double-float a b))
  (let* ((high (* a b))
         (low (fma a b (- high))))
    (values high low)))

;;; Multiply a by the sum of b-high and b-low.
(defun s-multiply (a b-high b-low)
  (declare (type double-float a b-high b-low))
  (multiple-value-bind (high low) (a-multiply a b-high) ; exact
    (values high (fma a b-low low))))

;;; Compute (- (* (+ a-high a-low) (b-high b-low)) (* a-low b-low))
;;; This code is a direct translation of the C code in the core-math
;;; library.  The comment in that library says that we can ignore the
;;; product of a-low and b-low if we assume that a-low is less than or
;;; equal to ulp(a-high) and b-low is less than or equal to
;;; ulp(b-high).
(defun d-multiply (a-high a-low b-high b-low)
  (declare (type double-float a-high a-low b-high b-low))
  (multiple-value-bind (high s) (a-multiply a-high b-high)
    (let* ((tt (fma a-low b-high s))
           (low (fma a-high b-low tt)))
      (values high low))))
