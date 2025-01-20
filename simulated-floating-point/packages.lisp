(cl:in-package #:common-lisp-user)

(defpackage #:buoy-simulate
  (:use #:common-lisp)
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
