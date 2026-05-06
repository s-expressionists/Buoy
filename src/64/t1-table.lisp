(cl:in-package #:buoy-core-math-64)

(defparameter *t1-table*
  (make-array
   '(64 2)
   :element-type 'double-float
   :initial-contents
   ;; Start by getting a ratioinal approximation of (expt 2 (/ 64))
   (let* ((long-base (buoy-simulate:rational-exp (* (/ 4096) (buoy-simulate:rational-ln 2))))
          (base (buoy-simulate:floatr-from-rational long-base 10 256)))
     (loop for i from 0 below 64
           collect (multiple-value-bind (high low)
                       (make-t-table-element base i)
                     (list high low))))))
