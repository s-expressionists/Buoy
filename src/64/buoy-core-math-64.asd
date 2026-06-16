(cl:in-package #:asdf-user)

(defsystem "buoy-core-math-64"
  :depends-on ("quaviver"
               "buoy-simulate")
  :serial t
  :components
  ((:file "packages")
   (:file "parse-c-literal")
   (:file "utilities")
   (:file "pi-rational")
   (:file "custom-float")
   (:file "sin-table")
   (:file "cos-table")
   (:file "polynomial-sine")
   (:file "polynomial-cosine")
   (:file "sine-cosine-table")
   (:file "t0-table")
   (:file "t1-table")
   (:file "reduce")
   (:file "sin")
   (:file "cos")
   (:file "log")
   (:file "asin")
   (:file "exp")
   #+(or)(:file "sinh")))
