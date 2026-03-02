(cl:in-package #:buoy-simulate)

(defun least-positive-normal-exponent (exponent-width)
  (- 2 (ash 1 (1- exponent-width))))

(defun least-positive-normal-floatr (exponent-width)
  (/ (ash 1 (- (ash 1 (1- exponent-width)) 2))))

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
            ;; For normal floatrs, we always want to return a first
            ;; value with an INTEGER-LENGTH of 24, which is the 23
            ;; bits of the represented mantissa plus the implicit 1.
            (let* ((length (integer-length numerator))
                   (diff (- 24 length))
                   (shifted (ash numerator diff)))
              (if (minusp diff)
                  ;; When the difference is negative, this means that
                  ;; the INTEGER-LENGTH of the numerator is greater
                  ;; than 24, so we have a large integer and the
                  ;; denominator is then 1.  So we return the shifted
                  ;; numerator and the negative value of the diff.
                  (values shifted
                          (- diff))
                  ;; when the difference is non-negative, we have a
                  ;; floatr with a value that has a denominator that
                  ;; is greater than or equal to 1.  The exponent
                  ;; represented by the denominator is again (L-1)
                  ;; where L is the INTEGER-LENGTH of the denominator.
                  ;; Since we shifted the numerator left (i.e., we
                  ;; made it greater) we need to make the denominator
                  ;; greater as well, so we return -((L-1) + diff)
                  ;; which is 1 - L - diff.
                  (values shifted
                          (- 1 (integer-length denominator) diff))))))))

(defun integer-decode-double-floatr (floatr)
  (if (zerop floatr)
      (values 0 0)
      (let ((numerator (numerator floatr))
            (denominator (denominator floatr)))
        (if (< floatr (/ (ash 1 1022)))
            ;; For subnormal floatrs, we always want to return -1074
            ;; as the second value, because -1074 = -1022 - 52 where
            ;; 52 is the number of bits in the mantissa.  So we need
            ;; to figure out the exponent represented by the
            ;; denominator, which is the INTEGER-LENGTH of the
            ;; denominator (call it L) minus 1.  So we need to shift
            ;; the denominator left by 1074 - (L-1) = 1075 - L.
            (let* ((length (integer-length denominator))
                   (diff (- 1075 length)))
              (values (ash numerator diff) -1074))
            ;; For normal floatrs, we always want to return a first
            ;; value with an INTEGER-LENGTH of 53, which is the 52
            ;; bits of the represented mantissa plus the implicit 1.
            (let* ((length (integer-length numerator))
                   (diff (- 53 length))
                   (shifted (ash numerator diff)))
              (if (minusp diff)
                  ;; When the difference is negative, this means that
                  ;; the INTEGER-LENGTH of the numerator is greater
                  ;; than 53, so we have a large integer and the
                  ;; denominator is then 1.  So we return the shifted
                  ;; numerator and the negative value of the diff.
                  (values shifted
                          (- diff))
                  ;; when the difference is non-negative, we have a
                  ;; floatr with a value that has a denominator that
                  ;; is greater than or equal to 1.  The exponent
                  ;; represented by the denominator is again (L-1)
                  ;; where L is the INTEGER-LENGTH of the denominator.
                  ;; Since we shifted the numerator left (i.e., we
                  ;; made it greater) we need to make the denominator
                  ;; greater as well, so we return -((L-1) + diff)
                  ;; which is 1 - L - diff.
                  (values shifted
                          (- 1 (integer-length denominator) diff))))))))
