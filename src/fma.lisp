(cl:in-package #:buoy)

;; portable implementation of fused multiply-add.  Should be replaced with a less pathological software implementation

;; (fma accumulator multiplier multiplicand) is (+ accumulator (* multiplier multiplicand)), rounding once
(declaim (ftype (function (single-float single-float single-float) single-float) fma-single)
         (ftype (function (double-float double-float double-float) double-float) fma-double))
#-(and sbcl (or x86-64 arm64))
(progn
  (defun fma-single (accumulator multiplier multiplicand)
    (float (+ (rational accumulator) (* (rational multiplier)
                                        (rational multiplicand)))
           0s0))
  (defun fma-double (accumulator multiplier multiplicand)
    (float (+ (rational accumulator) (* (rational multiplier)
                                        (rational multiplicand)))
           0d0)))
