(cl:in-package #:asdf-user)

#.(defparameter *buoy-simulate-documentation*
    (format nil "The purpose of this system is to simulate~@
                 floating-point operations using rationals.~@
                 A rational that has the exact value of some~@
                 floating-point datum is called a `floatr'.~@
                 This library has code to turn any rational~@
                 into a floatr with some given parameters, i.e.,~@
                 the width of the exponent field and the width~@
                 of the mantissa field.~@
                 ~@
                 The main use of this system is for bootstrapping~@
                 a Common Lisp system (the target) on some, perhaps~@
                 different, Common Lisp system (the host).  In such~@
                 a situation, the target system and the host system~@
                 may have different representations for their floating-~@
                 point data.  But the compiler must handle floating-~@
                 point data in the form of literals being created by~@
                 the reader and by operations executed at compile time.~@
                 It must also be able to apply operations such as~@
                 constant folding at compile time.~@
                 ~@
                 Clearly, representing floating-point data as floatrs~@
                 is not very fast.  But this library is not meant to~@
                 be used at run time, and floating-point operations~@
                 at bootstrapping time are typically very rare."))

(defsystem "buoy-simulate"
  :serial t
  :description "Simulated floating-point operations"
  :long-description #.*buoy-simulate-documentation*
  :components
  ((:file "packages")
   (:file "floatr")
   (:file "floatr-from-rational")
   (:file "floatr-precision")
   (:file "decode-floatr")
   (:file "integer-decode-floatr")
   (:file "square-root")
   (:file "exponentiation")
   (:file "logarithm")
   (:file "pi")
   (:file "sine-cosine")
   (:file "tangent")
   (:file "arcsine")
   (:file "hyperbolic-sine")))
