(cl:in-package #:buoy-core-math-64)

;;; Approximate (mod (/ X (* 2 PI)) 1).  X is a custom-float-64 used
;;; both as the input and the output.  Let Xin be the value of X when
;;; the function is called, and let Xout be the value of X when the
;;; function exits.  Then (< (ABS (- Xout (MOD (/ Xin (* 2 PI)) 1)))
;;; (* (EXPT 2 -126.67 (ABS Xout))))

(defun reduce1 (x)
  (when (<= (exponent x) 1) ; Then (< (ABS x) 2)
    ;; Multiply by (+ (/ (aref *pi-table* 0) (expt 2 64)) (/ (aref
    ;; *pi-table* (expt 2 128)))).  These table entries are the first
    ;; "digits" of (/ (* 2 PI)), with an error less than (expt 2
    ;; -130.22).
    (let* ((u (* (high x) (aref *pi-table* 1)))
           (tiny (ldb (byte 64 0) u)))
      (setf (low x) (ash u -64))
      (setf u (* (high x) (aref *pi-table* 0)))
      (let ((u-low (ldb (byte 64 0) u)))
        (incf (low x) u-low)
        (setf (high x) (+ (ash u -64) (if (< (low x) u-low) 1 0))))
      )))
