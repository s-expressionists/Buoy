(cl:in-package #:buoy-simulate)

(defun rational-tangent (argument)
  (/ (rational-sine argument)
     (rational-cosine argument)))
