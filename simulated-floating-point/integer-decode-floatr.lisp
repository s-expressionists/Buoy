(cl:in-package #:buoy-simulate)

(defun least-positive-normal-floatr (exponent-width)
  (/ (ash 1 (- (least-normal-exponent exponent-width)))))

;;; For subnormal floatrs, we always want to return the least positive
;;; normal exponent minus the mantissa width, since we are going to
;;; shift the mantissa by mantissa-width positions.
(defun subnormal-exponent-return-value (mantissa-width exponent-width)
  (- (least-normal-exponent exponent-width) mantissa-width))

(defun integer-decode-floatr (floatr mantissa-width exponent-width)
  (if (zerop floatr)
      (values 0 0)
      (let ((numerator (numerator floatr))
            (denominator (denominator floatr)))
        (if (< floatr (least-positive-normal-floatr exponent-width))
            ;; For subnormal floatrs, we always want to return
            ;; subnormal-exponent-return-value (call it E) as the
            ;; second value.  So we need to figure out the exponent
            ;; represented by the denominator, which is the
            ;; INTEGER-LENGTH of the denominator (call it L) minus 1.
            ;; So we need to shift the denominator left by
            ;; -E - (L-1) which is 1 - E - L
            (let* ((length (integer-length denominator))
                   (exponent
                     (subnormal-exponent-return-value
                      mantissa-width exponent-width))
                   (diff (- 1 exponent length)))
              (values (ash numerator diff) exponent))
            ;; For normal floatrs, we always want to return a first
            ;; value with an INTEGER-LENGTH of 1 + MANTISSA-WIDTH
            ;; which is MANTISSA-WIDTH bits of the represented
            ;; mantissa plus the implicit 1.
            (let* ((length (integer-length numerator))
                   (diff (- (1+ mantissa-width) length))
                   (shifted (ash numerator diff)))
              (if (minusp diff)
                  ;; When the difference is negative, this means that
                  ;; the INTEGER-LENGTH of the numerator is greater
                  ;; than 1 + MANTISSA-WIDTH, so we have a large
                  ;; integer and the denominator is then 1.  So we
                  ;; return the shifted numerator and the negative
                  ;; value of the diff.
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
