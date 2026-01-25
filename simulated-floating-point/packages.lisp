(cl:in-package #:common-lisp-user)

(defpackage #:buoy-simulate
  (:use #:common-lisp)
  (:export
   #:binary32-from-rational
   #:value
   #:binary32-+
   #:binary32--
   #:binary32-*
   #:binary32-/
   #:binary64-from-rational
   #:binary64-+
   #:binary64--
   #:binary64-*
   #:binary64-/))
