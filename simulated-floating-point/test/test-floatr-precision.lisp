(cl:in-package #:buoy-simulate-test)

(defun test-floatr-precison ()
  (loop for integer-mantissa from 0 below (ash 1 23)
        for floatr = (/ integer-mantissa (ash 1 (+ 126 23)))
        do (assert (= (buoy:floatr-precision floatr 23 8)
                      (integer-length integer-mantissa)))))
