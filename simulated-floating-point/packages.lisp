(cl:in-package #:common-lisp-user)

(defpackage #:buoy-simulate
  (:use #:common-lisp)
  (:export
   #:floatr-from-rational
   #:floatr32-from-rational
   #:floatr64-from-rational
   #:binary32-from-rational))
