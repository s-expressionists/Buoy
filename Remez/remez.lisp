(cl:in-package #:buoy-remez)

(defun create-matrix-row (function control-point degree error-coefficient)
  (append (loop for i from 0 to degree
                collect (expt control-point i))
          (list error-coefficient (funcall function control-point))))
  

(defun create-matrix (function control-points)
  (let* ((size (length control-points))
         (degree (- size 2)))
    (make-array
     (list size (1+ size))
     :initial-contents
     (loop for control-point in control-points
           for error-coefficient = 1 then (- error-coefficient)
           collect (create-matrix-row
                    function control-point degree error-coefficient)))))
