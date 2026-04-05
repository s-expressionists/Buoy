(cl:in-package #:buoy)

;;; This struct definition is used as a custom float type.  The value
;;; represented is (* (/ high 64) (/ low 128) (expt 2 (1+ exponent))).
;;; A sign slot of 0 means the value represented is positive and a
;;; sign slot of 1 mean the value represented is negative.

(defstruct custom-float-64
  (high 0 :type (unsigned-byte 64))
  (low 0 :type (unsigned-byte 64))
  (exponent 0 :type (signed-byte 64))
  (sign 0 :type (unsigned-byte 64)))

(defparameter *1*
  (make-custom-float-64
   :high #x8000000000000000
   :low #x0
   :exponent 0
   :sign 0))

(defparameter *-1*
  (make-custom-float-64
   :high #x8000000000000000
   :low #x0
   :exponent 0
   :sign 1))
