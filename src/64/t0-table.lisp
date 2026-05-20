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
(defun make-t0-table-element (rational-base i)
  (let ((pfloat-i (pf:pfloat-from-rational i))
        (pfloat-64 (pf:pfloat-from-rational (expt 2 6)))
        (pfloat-ln-2 (sim:pfloat-ln (pf:pfloat-from-rational 2)))
        (pfloat-exponent (pf:* (pf:/ pfloat-i pfloat-64) pfloat-ln-2))
        (pfloat-result (sim:pfloat-exp pfloat-exponent))
        (rational-result (pf:rational-from-pfloat pfloat-result))
        (high (dfloat rational-result))
        (low (dfloat (- rational-result (rational high)))))
    (values high low)))

(defparameter *t0-table*
  (make-array
   '(64 2)
   :element-type 'double-float
   :initial-contents
   ;; Start by getting a ratioinal approximation of (expt 2 (/ 64))
   (let ((base (loop repeat 7
                     for result = 2
                       then (sim:floatr-from-rational
                             (sim:rational-square-root result)
                             10 256)
                     finally (return result))))
     (loop for i from 0 below 64
           collect (multiple-value-bind (high low)
                       (make-t-table-element base i)
                     (list high low))))))
