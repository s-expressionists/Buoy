(cl:in-package #:buoy-remez)

(defun newton-step (x function derivative)
  (- x (/ (funcall function x) (funcall derivative x))))
