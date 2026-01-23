(cl:in-package #:buoy-simulate-test)

(defun normal-components-to-floatr (exponent stored-mantissa)
  (let ((real-mantissa (1+ (/ stored-mantissa (ash 1 23)))))
    (if (minusp exponent)
        (/ real-mantissa (ash 1 (- exponent)))
        (* real-mantissa (ash 1 exponent)))))

(defconstant +epsilon+ (/ (ash 1 1000)))

(defun test-normal-odd-even (lower upper)
  (let* ((average (/ (+ lower upper) 2))
         (result1 (buoy:binary32-from-rational average))
         (result2 (buoy:binary32-from-rational (+ average +epsilon+)))
         (result3 (buoy:binary32-from-rational (- average +epsilon+))))
    ;; It should round to even
    (assert (= (buoy:value result1) upper))
    (assert (= (buoy:value result2) upper))
    (assert (= (buoy:value result3) lower))))

(defun test-normal-even-odd (lower upper)
  (let* ((average (/ (+ lower upper) 2))
         (result1 (buoy:binary32-from-rational average))
         (result2 (buoy:binary32-from-rational (+ average +epsilon+)))
         (result3 (buoy:binary32-from-rational (- average +epsilon+))))
    ;; It should round to even
    (assert (= (buoy:value result1) lower))
    (assert (= (buoy:value result2) upper))
    (assert (= (buoy:value result3) lower))))

(defun test-normal-with-stored-mantissa (exponent stored-mantissa)
  (let ((next-stored-mantissa (1+ stored-mantissa)))
    (let* ((shifted-mantissa (+ stored-mantissa (ash 1 23)))
           (mantissa (/ shifted-mantissa (ash 1 23)))
           (lower (if (minusp exponent)
                      (/ mantissa (ash 1 (- exponent)))
                      (* mantissa (ash 1 exponent))))
           (next-shifted-mantissa (+ next-stored-mantissa (ash 1 23)))
           (next-mantissa (/ next-shifted-mantissa (ash 1 23)))
           (upper (if (minusp exponent)
                      (/ next-mantissa (ash 1 (- exponent)))
                      (* next-mantissa (ash 1 exponent)))))
      (if (oddp stored-mantissa)
          (test-normal-odd-even lower upper)
          (test-normal-even-odd lower upper)))))

(defun test-normals-with-exponent (exponent)
  (loop for stored-mantissa from 0 below (1- (ash 1 23))
        do (test-normal-with-stored-mantissa exponent stored-mantissa)))

(defun test-normals ()
  (loop for exponent from -126 to 128
        do (test-normals-with-exponent exponent)))
