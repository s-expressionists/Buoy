(cl:in-package #:buoy-simulate)

(defun floatr-precision (floatr exponent-width)
  (if (>= floatr (least-positive-normal-floatr exponent-width))
      ;; We have a normal floatr, so we return the width of the
      ;; exponent field plus 1 for the implicit bit.
      (1+ exponent-width)
      ;; We have a subnomal floatr.  Let's say that the INTEGER-LENGTH
      ;; of the denominator is L, so that it denotes (EXPT 2 (1- L)).
      ;; Because rationals are canonicalized, it might be that the L-1
      ;; is less than (- EXPONENT-WIDTH (LEAST-NORMAL-EXPONENT
      ;; EXPONENT-WIDTH)).  Then the numerator must be shifted left by
      ;; the difference to give the integer value of the mantissa.
      ;; But we don't have to do the shifting.  We can take the
      ;; INTEGER-LENGTH of the numerator and just add the difference
      ;; to it.  The difference is (- (- L 1) EXPONENT-WIDTH (-
      ;; (LEAST-NORMAL-EXPONENT EXPONENT-WIDTH))) which is (- L 1
      ;; EXPONENT-WIDTH (- (LEAST-NORMAL-EXPONENT EXPONENT-WIDTH))).
      (+ (integer-length (numerator floatr))
         (- (integer-length (denominator floatr))
            1
            exponent-width
            (- (least-normal-exponent exponent-width))))))
