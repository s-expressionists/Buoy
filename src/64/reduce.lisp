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
        ;; Let HI be the current value of (high x) and HI-IN the value
        ;; of (high x) when this function was entered.  Let LO be the
        ;; value of (low x).  Also, let PI-0 be the (AREF PT 0) and
        ;; PI-1 (AREF pt 1). Then (+ HI (/ lo (expt 2 64)) (/ tiny
        ;; (expt 2 128))) is equal to (* HI-IN (+ (/ PI-0 (expt 2 64))
        ;; (/ PI-1 (expt 2 128).  Thus (< (abs (- (+ HI (/ lo (expt 2
        ;; 64)) (/ tiny (expt 2 128))) (/ HI-IN (* 2 PI)))) (* HI-IN
        ;; (expt 2 -130.22))).  Since X is normalized at input, (>=
        ;; HI-IN (expt 2 63)) and since (>= (AREF PT 0) (expt 2 61)),
        ;; we have (>= HI (expt 2 (- (+ 63 61) 64))) which is (expt 2
        ;; 60).  Thus the call to NORMALIZE below performs a left
        ;; shift by at most 3 bits
        (let ((e (exponent x)))
          (normalize-custom-float-64 x)
          (decf e (exponent x))
          (unless (zerop e)
            (setf (low x) (logior (low x) (ash tiny (- 64 e)))))
          (return-from reduce1)
          ;; Now (<= 2 e 1024)
          ;;
          ;; FIXME: adapt this comment.
          ;; The upper 64-bit word X->hi corresponds to hi/2^64*2^e,
          ;; if multiplied by T[i]/2^((i+1)*64) it yields
          ;; hi*T[i]/2^128 * 2^(e-i*64).  If e-64i <= -128, it
          ;; contributes to less than 2^-128; if e-64i >= 128, it
          ;; yields an integer, which is 0 modulo 1.  We thus only
          ;; consider the values of i such that -127 <= e-64i <= 127,
          ;; i.e., (-127+e)/64 <= i <= (127+e)/64.  Up to 4
          ;; consecutive values of T[i] can contribute (only 3 when e
          ;; is a multiple of 64).
          (let ((i (if (< e 127) 0 (ceiling (- e 127) 64)))
                (c (make-array 5 :element-type 'double-float)))
          ))))))
