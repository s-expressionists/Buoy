(cl:in-package #:common-lisp-user)

(defpackage #:buoy-simulate
  (:use #:common-lisp)
  (:shadow
   #:most-positive-single-float
   #:most-negative-single-float
   #:least-positive-single-float
   #:least-positive-normalized-single-float
   #:least-negative-single-float
   #:least-negative-normalized-single-float
   #:most-positive-double-float
   #:most-negative-double-float
   #:least-positive-double-float
   #:least-positive-normalized-double-float
   #:least-negative-double-float
   #:least-negative-normalized-double-float)
  (:export
   #:single-float-from-components
   #:single-float-from-rational
   #:rational-from-single-float
   #:single-float-binary-+
   #:single-float-binary--
   #:single-float-binary-*
   #:single-float-binary-/
   #:double-float-from-components
   #:double-float-from-rational
   #:rational-from-double-float
   #:double-float-binary-+
   #:double-float-binary--
   #:double-float-binary-*
   #:double-float-binary-/))
