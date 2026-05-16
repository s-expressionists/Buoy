(cl:in-package #:common-lisp-user)

(defpackage #:buoy-pfloat
  (:use #:common-lisp)
  (:shadow . #1=(#:+ #:- #:* #:/ #:< #:= #:minusp #:zerop))
  (:export #:make-pfloat
           #:mantissa
           #:exponent
           #:pfloat-from-rational
           #:rational-from-pfloat
           #:double-float-from-pfloat
           #:negate
           #:*precision*
           #:*zero*
           #:*one*
           #:*two*
           . #1#))

(defpackage #:buoy-simulate
  (:use #:common-lisp)
  (:local-nicknames (#:pf #:buoy-pfloat))
  (:export
   #:floatr-from-rational
   #:floatr32-from-rational
   #:floatr64-from-rational
   #:binary32-from-rational
   #:floatr-precision
   #:decode-floatr
   #:integer-decode-floatr
   #:dfloat
   #:rational-ln
   #:rational-exp
   #:*pi*
   #:rational-square-root
   #:rational-sine
   #:rational-cosine
   #:rational-tangent
   #:rational-arcsine
   #:rational-hyperbolic-sine
   #:rational-hyperbolic-cosine))
