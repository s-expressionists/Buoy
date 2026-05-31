(cl:in-package #:buoy-simulate)

(defun pfloat-area-hypberbolic-tangent (pfloat)
  (let* ((sum (pf:+ pf:*one* pfloat))
         (difference (pf:- pf:*one* pfloat))
         (quotient (pf:/ sum difference))
         (log (pfloat-ln quotient)))
    (pf:/ log pf:*two*)))
