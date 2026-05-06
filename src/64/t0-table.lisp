(cl:in-package #:buoy-core-math-64)

;;; This table is used in several places, most of which have no
;;; comment associated with it.  Only in exp/exp.c there is the
;;; following comment:
;;;
;;; for 0 <= i < 2^6, t0[i] is a double-double approximation of 2^(i/2^6)
;;;
;;; As for the name, I don't know what it means.

;;; We can't use precise rational calculations until the end, because
;;; it takes to long to compute.  So we round to a 256-bit value in
;;; each iteration.
;;;
;;; Oh, and in the table in core-math the first element is the low
;;; value and the second element is the high value.
(defun make-t0-table-element (rational-base i)
  (loop repeat (1+ i)
        for result = 1
          then (buoy-simulate:floatr-from-rational
                (* result rational-base)
                10 256)
        finally (let* ((high (dfloat result))
                       (remaining (- result (rational high)))
                       (low (dfloat remaining)))
                  (return (values low high)))))

(defparameter *t0-table*
  (make-array
   '(64 2)
   :element-type 'double-float
   :initial-contents
   ;; Start by getting a ratioinal approximation of (expt 2 (/ 64))
   (let ((base (loop repeat 7
                     for result = 2
                       then (buoy-simulate:floatr-from-rational
                             (buoy-simulate:rational-square-root result)
                             10 256)
                     finally (return result))))
     (loop for i from 0 below 64
           collect (multiple-value-bind (high low)
                       (make-t0-table-element base i)
                     (list high low))))))
     
              
