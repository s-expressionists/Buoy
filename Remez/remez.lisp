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

(defun extract-polynomial-coefficients (solved-system)
  (destructuring-bind (rows columns)
      (array-dimensions solved-system)
    (loop for row from 0 below (1- rows)
          collect (aref solved-system row (1- columns)))))

(defun create-polynomial (coefficients)
  (compile nil
           `(lambda (x)
              (+ ,@(loop for variables = '() then (cons 'x variables)
                         for coefficient in coefficients
                         collect `(* ,coefficient ,@variables))))))

(defun derivative-coefficients (coefficients)
  (loop for coefficient in (rest coefficients)
        for i from 1
        collect (* coefficient i)))
