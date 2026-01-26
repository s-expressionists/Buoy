(cl:in-package #:buoy-simulate)

;;; We define a FLOATR to be a rational value that is the exact value
;;; of some floating-point number.

;;; The least positive infinite floatr is a rational that is the
;;; representation of a floating point value that can be constructed
;;; as the maximum exponent allowed for normal floating-point values,
;;; and with a mantissa 1.111... where the number of 1s following the
;;; period is one more than what the representation allows.  If an
;;; attempt is made to round this mantissa to an even value, then the
;;; result will be larger than the largest normal floater.  A value
;;; only slightly less than this will round to the largest positive
;;; normal floatr.


; LocalWords:  floatr
