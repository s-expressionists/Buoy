(cl:in-package #:buoy-simulate)

(defun pfloat-area-hyperbolic-sine (pfloat)
  (let* ((square (pf:* pfloat pfloat))
         (square-root (pfloat-square-root (pf:+ square pf:*one*))))
    (pfloat-ln (pf:+ pfloat square-root))))
