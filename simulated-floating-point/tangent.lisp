(cl:in-package #:buoy-simulate)

(defun rational-tangent (argument)
  (/ (rational-sine argument)
     (rational-cosine argument)))

;;; We could do something faster here, but this should be plenty fast
;;; for use at compile time and for generating core-math tables.

(defun pfloat-tangent (pfloat)
  (/ (pfloat-sine pfloat)
     (pfloat-cosine pfloat)))
