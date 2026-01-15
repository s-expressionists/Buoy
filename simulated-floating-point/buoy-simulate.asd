(cl:in-package #:asdf-user)

(defsystem "buoy-simulate"
  :serial t
  :components
  ((:file "packages")
   (:file "binary32")
   (:file "single-float")
   (:file "double-float")
   (:file "conversion")))
