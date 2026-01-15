(cl:in-package #:buoy-simulate)

;;; This is not quite true. 
(defconstant normal-binary32-must-be-less
  (ash 1 127))

;;; This is not quite true. 
(defconstant normal-binary32-must-be-greater
  (- (ash 1 127)))

(defclass binary32 ()
  ())

(defclass binary32-normal-or-subnormal (binary32)
  ((%value :initarg :value :reader value)))

(defclass binary32-normal (binary32-normal-or-subnormal)
  ())

(defclass binary32-subnormal (binary32-normal-or-subnormal)
  ())

(defclass binary32-infinity (binary32)
  ())

(defclass binary32-nan (binary32)
  ())
