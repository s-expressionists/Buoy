(cl:in-package #:buoy-simulate)

(defun pfloat-hyperbolic-sine (pfloat)
  (cond ((pf:zerop pfloat)
         pf:*zero*)
        ((pf:minusp pfloat)
         (let ((exp (pfloat-exp (pf:negate pfloat))))
           (pf:/ (pf:- (pf:/ pf:*one* exp) exp) pf:*two*)))
        (t
         (let ((exp (pfloat-exp pfloat)))
           (pf:/ (pf:- exp (pf:/ pf:*one* exp)) pf:*two*)))))
