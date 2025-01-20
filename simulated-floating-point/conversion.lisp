(cl:in-package #:buoy-simulate)

(defun double-float-from-single-float (single-float)
  (double-float-from-rational
   (rational-from-single-float single-float)))

(defun single-float-from-double-float (double-float)
  (single-float-from-rational
   (rational-from-double-float double-float)))
