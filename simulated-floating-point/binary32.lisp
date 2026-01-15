(cl:in-package #:buoy-simulate)

;;; This is not quite true. 
(defconstant ratio-must-be-less-for-normal-binary32
  (ash 1 127))

;;; This is not quite true. 
(defconstant ratio-must-be-greater-for-normal-binary32
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

(defun binary32-from-rational (rational)
  (let ((exponent 0)
        (sign (if (minusp rational) -1 1))
        (mantissa (if (minusp rational) (- rational) rational)))
    (cond 
