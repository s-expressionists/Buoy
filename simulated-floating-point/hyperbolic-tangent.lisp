(cl:in-package #:buoy-simulate)

(defun pfloat-hyperbolic-tangent (pfloat)
  (pf:/ (pfloat-hyperbolic-sine pfloat)
        (pfloat-hyperbolic-cosine pfloat)))
