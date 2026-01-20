(cl:in-package #:buoy-simulate-test)

(defun normal-components-to-floatr (exponent stored-mantissa)
  (let ((real-mantissa (1+ (/ stored-mantissa (ash 1 23)))))
    (if (minusp exponent)
        (/ real-mantissa (ash 1 (- exponent)))
        (* real-mantissa (ash 1 exponent)))))

(defconstant +epsilon+ (/ (ash 1 1000)))

(defun test-normal-odd-even (lower upper)
  (let* ((average (/ (+ lower upper) 2))
         (result1 (buoy:binary-32-from-rational average))
         (result2 (buoy:binary-32-from-rational (+ average +epsilon+)))
         (result3 (buoy:binary-32-from-rational (- average +epsilon+))))
    ;; It should round to even
    (assert (= result1 upper))
    (assert (= result2 upper))
    (assert (= result3 lower))))
    
  
