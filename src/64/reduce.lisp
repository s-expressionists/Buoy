(cl:in-package #:buoy-core-math-64)

;;; Approximate (mod (/ X (* 2 PI)) 1).  X is a custom-float-64 used
;;; both as the input and the output.  Let Xin be the value of X when
;;; the function is called, and let Xout be the value of X when the
;;; function exits.  Then (< (ABS (- Xout (MOD (/ Xin (* 2 PI)) 1)))
;;; (* (EXPT 2 -126.67 (ABS Xout))))

(defun reduce1 (x)
  (let ((pt *pi-table*))
    (when (<= (exponent x) 1) ; Then (< (ABS x) 2)
      ;; Multiply by (+ (/ (aref pt 0) (expt 2 64)) (/ (aref pt (expt
      ;; 2 128)))).  These table entries are the first "digits" of (/
      ;; (* 2 PI)), with an error less than (expt 2 -130.22).
      (let* ((u (* (high x) (aref pt 1)))
             (tiny (ldb (byte 64 0) u)))
        (setf (low x) (ash u -64))
        (setf u (* (high x) (aref pt 0)))
        (let ((u-low (ldb (byte 64 0) u)))
          (incf (low x) u-low)
          (setf (high x) (+ (ash u -64) (if (< (low x) u-low) 1 0))))
        ;; Let HI be the value of (high x) and HI-IN the value of
        ;; (high x) when this function was entered.  Let LO be the
        ;; value of (low x).  Also, let PI-0 be the (AREF PT 0) and
        ;; PI-1 (AREF pt 1). Then (+ HI (/ lo (expt 2 64)) (/ tiny
        ;; (expt 2 128))) is equal to (* HI-IN (+ (/ PI-0 (expt 2 64))
        ;; (/ PI-1 (expt 2 128).
        ))))
