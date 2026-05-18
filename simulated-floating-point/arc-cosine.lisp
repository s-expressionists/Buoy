(cl:in-package #:buoy-simulate)

(defparameter *pfloat-pi*
  (pf:pfloat-from-rational *pi*))

(defparameter *pfloat-pi/2*
  (pf:pfloat-from-rational (/ *pi* 2)))

(defun pfloat-arccos (pfloat)
  (pf:- *pfloat-pi/2* (pfloat-arcsine pfloat)))
