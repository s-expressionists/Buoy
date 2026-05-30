(cl:in-package #:buoy-simulate)

(defparameter *arc-tangent-factors*
  (cons pf:*zero*
        (loop for i from 3 by 2
              repeat 100
              collect (pf:pfloat-from-rational (/ (- i 2) i)))))

(defun pfloat-arc-tangent-with-small-ish-positive-argument (pfloat)
  (loop with sum = pf:*zero*
        with square = (pf:* pfloat pfloat)
        for factor in *arc-tangent-factors*
        for term = pfloat then (pf:negate (pf:* (pf:* term square) factor))
        for sum2 = term then (pf:+ sum term)
        until (pf:= sum sum2)
        do (setf sum sum2)
        finally (return sum)))

(defun pfloat-arc-tangent-with-positive-argument (pfloat)
  (let* ((square-root (pfloat-square-root (pf:+ pf:*one* (pf:* pfloat pfloat))))
         (small-ish-argument (pf:/ pfloat (pf:+ pf:*one* square-root))))
    (pf:* pf:*two*
          (pfloat-arc-tangent-with-small-ish-positive-argument
           small-ish-argument))))
