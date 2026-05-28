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
           #:restrict-to-ieee-precision
           #:restrict-to-ieee-single
           #:restrict-to-ieee-double
           . #1#))

(defpackage #:buoy-simulate
  (:use #:common-lisp)
  (:local-nicknames (#:pf #:buoy-pfloat))
  (:export
   #:binary32-from-rational
   #:dfloat
   #:pfloat-ln
   #:rational-ln
   #:pfloat-exp
   #:rational-exp
   #:rational-expt
   #:*pi*
   #:rational-square-root
   #:pfloat-sine
   #:rational-sine
   #:pfloat-cosine
   #:rational-cosine
   #:rational-tangent
   #:rational-arcsine
   #:rational-hyperbolic-sine
   #:rational-hyperbolic-cosine))
