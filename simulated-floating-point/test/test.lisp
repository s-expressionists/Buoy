(cl:in-package #:buoy-simulate-test)

(defun normal-components-to-floatr (exponent stored-mantissa)
  (let ((real-mantissa (1+ (/ stored-mantissa (ash 1 23)))))
    (if (minusp exponent)
        (/ real-mantissa (ash 1 (- exponent)))
        (* real-mantissa (ash 1 exponent)))))
