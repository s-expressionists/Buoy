(cl:in-package #:asdf-user)

#.(defparameter *buoy-simulate-documentation*
    (format nil ""))

(defsystem "buoy-simulate"
  :serial t
  :description "Simulated floating-point operations"
  :long-description #.*buoy-simulate-documentation*
  :components
  ((:file "packages")
   (:file "dfloat")
   (:file "pfloat")
   (:file "square-root")
   (:file "exponentiation")
   (:file "logarithm")
   (:file "pi")
   (:file "sine-cosine")
   (:file "tangent")
   (:file "arc-sine")
   (:file "arc-cosine")
   (:file "arc-tangent")
   (:file "hyperbolic-sine")
   (:file "hyperbolic-cosine")
   (:file "hyperbolic-tangent")
   (:file "area-hyperbolic-sine")))
