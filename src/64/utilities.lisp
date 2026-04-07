(cl:in-package #:buoy-core-math-64)

(defun fast-two-sum (a b)
  (declare (type double-float a b))
  (let* ((high (+ a b))
         (err (- high a)) ; exact
         (low (- b err))) ; exact
    (values high low)))
