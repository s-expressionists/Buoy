(cl:in-package #:asdf-user)

(defsystem "buoy-simulate"
  :serial t
  :components
  ((:file "packages")
   (:file "single-float")))
