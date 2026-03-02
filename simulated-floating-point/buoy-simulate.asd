(cl:in-package #:asdf-user)

(defsystem "buoy-simulate"
  :serial t
  :components
  ((:file "packages")
   (:file "floatr")
   (:file "decode-floatr")
   (:file "integer-decode-floatr")))
