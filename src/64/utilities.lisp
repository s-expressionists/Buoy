(cl:in-package #:buoy-core-math-64)

;;; The exact sum of HIGH and LOW is the exact sum of A and B.  One
;;; might wonder what the purpose is of taking two arguments the sum
;;; of which is (say) S, and return two values the sum of which is
;;; also S.  But the thing is that the second return value is smaller
;;; than the least significant bit in the first return value.  Here is
;;; how it works: Suppose B is smaller than A (I am not sure it works
;;; the other way around), then part of B will not be taken into
;;; account in the sum.  Consider B to be C + D, where C is the part
;;; of B that will be part of the floating-point sum of A and B, so
;;; that HIGH is A + C.  Then if you compute HIGH - A, you will get C
;;; exactly.  Then if you compute B - C, you will get D exactly.  But
;;; since D was to small to influence HIGH, it means that D is smaller
;;; than the least significant bit of HIGH.
(defun fast-two-sum (a b)
  (declare (type double-float a b))
  (let* ((high (+ a b))
         (err (- high a)) ; exact
         (low (- b err))) ; exact
    (declare (type double-float high low err))
    ;; Now hi + lo = a + b exactly for rounding to nearest.  For
    ;; directed rounding modes, this is not always true.  Take for
    ;; example a = 1, b = 2^-200, and rounding up, then hi = 1 +
    ;; 2^-52, e = 2^-52 (it can be proven that e is always exact), and
    ;; lo = -2^52 + 2^-105, thus hi + lo = 1 + 2^-105 <> a + b = 1 +
    ;; 2^-200.  A bound on the error is given in "Note on FastTwoSum
    ;; with Directed Roundings" by Paul Zimmermann,
    ;; https://hal.inria.fr/hal-03798376, 2022.  Theorem 1 says that
    ;; the difference between a+b and hi+lo is bounded by 2u^2|a+b|
    ;; and also by 2u^2|hi|. Here u=2^-53, thus we get:
    ;; |(a+b)-(hi+lo)| <= 2^-105 min(|a+b|,|hi|)
    (values high low)))

(defun fast-sum (xh xl yh yl)
  (multiple-value-bind (sh sl)
      (fast-two-sum xh yh)
    (let ((e (+ (+ xl yl) sl)))
      (values sh e))))

;;; The core-math library uses a C99 function called `fma'.  It takes
;;; three arguments, x, y, and z and it returns (+ (* x y) z),
;;; apparently without losing any intermediate precision.  The only
;;; way I can think of implementing this in standard Common Lisp is to
;;; use rationals, but that won't be fast.  I also don't know how C99
;;; compilers generate code for it, unless the architecture has an
;;; instruction that does that.
(defun fma (x y z)
  (declare (type double-float x y z))
  (sb-simd-fma:f64-fmadd x y z)
  #+(or)(sim:dfloat (+ (* (rational x) (rational y))
                       (rational z))))

;;; Multiply exactly A and B such that the sum of HIGH and LOW is the
;;; exact product of A and B.  The core-math library call this
;;; function `a_mul' and I have no ideas for a much improved name.
(defun a-multiply (a b)
  (declare (type double-float a b))
  (let* ((high (* a b))
         (low (fma a b (- high))))
    (declare (type double-float high low))
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
      (declare (type double-float tt low))
      (values high low))))

;;; I don't know what the name stands for.
(defun multiply-dd (xh xl ch cl)
  (declare (type double-float xh xl ch cl))
  (let* ((h (* xh ch))
         (l (+ (fma xh ch (- h)) (*  xh cl) (* xl ch))))
    (values h l)))

(defun multiply-ddd (xh xl ch)
  (declare (type double-float xh xl ch))
  (let* ((h (* xh ch))
         (l (+ (fma xh ch (- h)) (* xl ch))))
    (values h l)))

(defun double-double-from-rational (rational)
  (let* ((high (dfloat rational))
         (diff (- rational (rational high)))
         (low (dfloat diff)))
    (values high low)))

(defun double-double-from-pfloat (pfloat)
  (double-double-from-rational (pf:rational-from-pfloat pfloat)))
