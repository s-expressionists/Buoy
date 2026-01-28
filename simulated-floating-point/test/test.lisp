(cl:in-package #:buoy-simulate-test)

(defun normal-components-to-floatr (exponent stored-mantissa)
  (let ((real-mantissa (1+ (/ stored-mantissa (ash 1 23)))))
    (if (minusp exponent)
        (/ real-mantissa (ash 1 (- exponent)))
        (* real-mantissa (ash 1 exponent)))))

(defconstant +epsilon+ (/ (ash 1 1000)))

(defun test-odd-even (lower upper)
  (let* ((average (/ (+ lower upper) 2))
         (result1 (buoy:floatr32-from-rational average))
         (result2 (buoy:floatr32-from-rational (+ average +epsilon+)))
         (result3 (buoy:floatr32-from-rational (- average +epsilon+))))
    ;; It should round to even
    (assert (= (identity result1) upper))
    (assert (= (identity result2) upper))
    (assert (= (identity result3) lower))))

(defun test-even-odd (lower upper)
  (let* ((average (/ (+ lower upper) 2))
         (result1 (buoy:floatr32-from-rational average))
         (result2 (buoy:floatr32-from-rational (+ average +epsilon+)))
         (result3 (buoy:floatr32-from-rational (- average +epsilon+))))
    ;; It should round to even
    (assert (= (identity result1) lower))
    (assert (= (identity result2) upper))
    (assert (= (identity result3) lower))))

(defun test-normal-with-stored-mantissa (exponent stored-mantissa)
  (let* ((next-stored-mantissa (1+ stored-mantissa))
         (lower (normal-components-to-floatr exponent stored-mantissa))
         (upper (normal-components-to-floatr exponent next-stored-mantissa)))
    (if (oddp stored-mantissa)
        (test-odd-even lower upper)
        (test-even-odd lower upper))))

(defun test-normals-with-exponent (exponent)
  (loop for stored-mantissa from 0 below (1- (ash 1 23))
        do (test-normal-with-stored-mantissa exponent stored-mantissa)))

(defun test-normals ()
  (loop for exponent from -126 to 128
        do (format *trace-output* "Testing exponent ~s~%" exponent)
        do (test-normals-with-exponent exponent)))

(defun subnormal-stored-mantissa-to-floatr (stored-mantissa)
  (/ stored-mantissa (ash 1 (+ 126 23))))

(defun test-subnormal-with-stored-mantissa (stored-mantissa)
  (let* ((next-stored-mantissa (1+ stored-mantissa))
         (lower (subnormal-stored-mantissa-to-floatr stored-mantissa))
         (upper (subnormal-stored-mantissa-to-floatr next-stored-mantissa)))
      (if (oddp stored-mantissa)
          (test-odd-even lower upper)
          (test-even-odd lower upper))))

(defun test-subnormals ()
  (loop for stored-mantissa from 0 below (1- (ash 1 23))
        do (test-subnormal-with-stored-mantissa stored-mantissa)))
