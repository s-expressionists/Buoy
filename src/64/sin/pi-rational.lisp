(cl:in-package #:buoy)

;;; I haven't actually computed the required precision for
;;; +pi-rational+, nor the number of required iterations in the
;;; Maclaurin series in order to get an accurate rational
;;; approximation of the sine of an angle.  It could be significantly
;;; less than I have here, which would make the table computation
;;; faster.
;;;
;;; Also, would it be advantageous to have the denominator be a power
;;; of 2 rather than a power of 10?

(defparameter +pi-rational+
  (let ((pi-string "31415926535897932384626433832795028841971"))
    (/ (read-from-string pi-string)
       (expt 10 (1- (length pi-string))))))
