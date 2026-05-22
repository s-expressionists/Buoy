(cl:in-package #:buoy-core-math-64)

;;; for 0 <= i < 2^6, t1[i] is a double-double approximation of
;;; 2^(i/2^12)

(defun make-t1-table-entry (i)
  (let* ((pfloat-i (pf:pfloat-from-rational i))
         (pfloat-4096 (pf:pfloat-from-rational (expt 2 12)))
         (pfloat-ln-2 (sim:pfloat-ln (pf:pfloat-from-rational 2)))
         (pfloat-exponent (pf:* (pf:/ pfloat-i pfloat-4096) pfloat-ln-2))
         (pfloat-result (sim:pfloat-exp pfloat-exponent))
         (rational-result (pf:rational-from-pfloat pfloat-result))
         (high (dfloat rational-result))
         (low (dfloat (- rational-result (rational high)))))
    (values high low)))

(defparameter *t1-table*
  (make-array
   '(64 2)
   :element-type 'double-float
   :initial-contents
   (loop for i from 0 below 64
         collect (multiple-value-bind (high low)
                     (make-t1-table-entry i)
                   (list high low)))))
