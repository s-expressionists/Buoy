(cl:in-package #:asdf-user)

(defsystem "buoy-core-math-64"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "pi-rational")
   (:file "custom-float")
   (:file "sin-table")
   (:file "cos-table")
   (:file "parse-c-literal")
   (:file "polynomial-sine")
   (:file "polynomial-cosine")
   (:file "sine-cosine-table")
   (:file "reduce")")))
