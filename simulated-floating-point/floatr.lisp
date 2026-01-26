(cl:in-package #:buoy-simulate)

;;; We define a FLOATR to be a rational value that is the exact value
;;; of some floating-point number.

(defun most-positive-normal-exponent (exponent-width)
  (1- (ash 1 (1- exponent-width))))

;;; The least positive infinite floatr is a rational that is the
;;; representation of a floating point value that can be constructed
;;; as the maximum exponent allowed for normal floating-point values,
;;; and with a mantissa 1.111... where the number of 1s following the
;;; period is one more than what the representation allows.  If an
;;; attempt is made to round this mantissa to an even value, then the
;;; result will be larger than the largest normal floater.  A value
;;; only slightly less than this will round to the largest positive
;;; normal floatr.
(defun least-positive-infinite-floatr (exponent-width mantissa-width)
  ;; We construction an integer contains a string of N 1s, where N is
  ;; the width of the mantissa plus 2 (plus 1 for the bit that is not
  ;; represented, and 1 for the additional bit at the end).
  (let ((ones (1- (ash 1 (+ mantissa-width 2)))))
    ;; To get the real mantissa from this integer, we must divide it
    ;; with (EXPT 2 M) such that the result is a number greater than 1
    ;; but less than 2.  For that to work, M must be equal to N-1
    ;; which is the width of the mantissa plus 1.
    (let ((mantissa (/ ones (ash 1 (1+ mantissa-width)))))
      ;; We must now multiply the mantissa and the most positive
      ;; normal exponent to get the final result
      (* mantissa (most-positive-normal-exponent exponent-width)))))

; LocalWords:  floatr
