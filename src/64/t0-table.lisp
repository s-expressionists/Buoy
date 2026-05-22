(cl:in-package #:buoy-core-math-64)

;;; This table is used in several places, most of which have no
;;; comment associated with it.  Only in exp/exp.c there is the
;;; following comment:
;;;
;;; for 0 <= i < 2^6, t0[i] is a double-double approximation of 2^(i/2^6)
;;;
;;; As for the name, I don't know what it means.
;;;
;;; Oh, and in the table in core-math the first element is the low
;;; value and the second element is the high value.
(defun make-t0-table-element (i)
  (let ((rational-result (sim:rational-expt 2 (/ i 64))))
    (double-double-from-rational rational-result)))

(defparameter *t0-table*
  (make-array
   '(64 2)
   :element-type 'double-float
   :initial-contents
   (loop for i from 0 below 64
         collect (multiple-value-bind (high low)
                       (make-t0-table-element i)
                   (list low high)))))
