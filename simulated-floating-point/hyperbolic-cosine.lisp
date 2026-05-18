(cl:in-package #:buoy-simulate)

(defun pfloat-hyperbolic-cosine (pfloat)
  (if (pf:zerop pfloat)
      pf:*one*)
  (let ((exp (if (pf:minusp pfloat)
                 (pfloat-exp (pf:negate pfloat))
                 (pfloat-exp pfloat))))
    (pf:/ (pf:+ (pf:/ pf:*one* exp) exp) pf:*two*)))
