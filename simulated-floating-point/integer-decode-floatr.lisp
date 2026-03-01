(cl:in-package #:buoy-simulate)

(defun integer-decode-single-floatr (floatr)
  (if (zerop floatr)
      (values 0 0)
      (let ((numerator (numerator floatr))
            (denominator (denominator floatr)))
        (if (< floatr (/ (ash 1 126)))
            ;; For subnormal floatrs, we always want to return -149 as
            ;; the second value, because -149 = -126 - 23 where 23 is
            ;; the number of bits in the mantissa.  So we need to
            ;; figure out the exponent represented by the denominator,
            ;; which is the INTEGER-LENGTH of the denominator (call it
            ;; L) minus 1.  So we need to shift the denominator left
            ;; by 149 - (L-1) = 150 - L.
            (let* ((length (integer-length denominator))
                   (diff (- 150 length)))
              (values (ash numerator diff) -149))
            (let* ((length (integer-length numerator))
                   (diff (- 24 length))
                   (shifted (ash numerator diff)))
              (if (minusp diff)
                  (values shifted
                          (- diff))
                  (values shifted
                          (- 1 (integer-length denominator) diff))))))))
