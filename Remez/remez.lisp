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

;;; We can't use Newton's method directly, because we may have an
;;; approximation where the derivative is close to 0.  So we need to
;;; find some approximate roots first, and then refine them
(defun find-approximate-roots (start end function)
  (loop for x from start to end by (* (- end start) 1d-3)
        for y1 = (funcall function x)
        for y2 = (funcall function (+ x 1d-3))
        when (not (plusp (* y1 y2)))
          collect x))

(defun refine-roots (roots function derivative)
  (loop for root in roots
        collect (loop for approximation = root
                        then (newton-step approximation function derivative)
                      repeat 20
                      finally (return approximation))))

(defun find-roots (start end function derivative)
  (refine-roots (find-approximate-roots start end function)
                function derivative))
