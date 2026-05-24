(cl:in-package #:asdf-user)

(defsystem "buoy-remez"
  :serial t
  :components
  ((:file "packages")
   (:file "solve-linear-system")
   (:file "newton")
   (:file "remez")))
