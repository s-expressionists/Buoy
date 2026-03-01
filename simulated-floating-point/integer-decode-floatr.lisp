(cl:in-package #:buoy-simulate)

(defun integer-decode-single-floatr (floatr)
  (let* ((numerator (numerator floatr))
         (length (integer-length numerator))
         (diff (- 24 length))
         (shifted (ash numerator diff)))
    (cond ((zerop floatr)
           (values 0 0))
          ((< floatr (/ (ash 1 126)))
           ;; We have a subnormal floatr.
           ;; FIXME: do this right.
           nil)
          ((minusp diff)
           (values shifted
                   (- diff)))
          (t
           (values shifted
                   (- 1 (integer-length (denominator floatr)) diff))))))
