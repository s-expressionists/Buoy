(cl:in-package #:asdf-user)

(defsystem "buoy-simulate-test"
  :depends-on ("buoy-simulate")
  :serial t
  :components
  ((:file "packages")
   (:file "test")))
