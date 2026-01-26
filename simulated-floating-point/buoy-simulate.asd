(cl:in-package #:asdf-user)

(defsystem "buoy-simulate"
  :serial t
  :components
  ((:file "packages")
   (:file "floatr")
   (:file "binary32")
   (:file "binary64")
   (:file "conversion")))
