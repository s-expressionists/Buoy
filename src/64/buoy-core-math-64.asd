(cl:in-package #:asdf-user)

(defsystem "buoy-core-math-64"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "pi-rational")))
