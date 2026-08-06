(cl:in-package #:buoy-core-math-64)

(defparameter *t-inv-table*
  (make-array
   256
   :element-type '(unsigned-byte 64)
   :initial-contents
   (loop for i from 0 below 256
         collect (floor (/ (expt 2 127)
                           (+ (expt 2 63)
                              (* i (expt 2 55))
                              (expt 2 55)
                              -1))))))
