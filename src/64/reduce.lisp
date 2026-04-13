(cl:in-package #:buoy-core-math-64)

;;; Approximate (mod (/ X (* 2 PI)) 1).  X is a custom-float-64 used
;;; both as the input and the output.  Let Xin be the value of X when
;;; the function is called, and let Xout be the value of X when the
;;; function exits.  Then (< (ABS (- Xout (MOD (/ Xin (* 2 PI)) 1)))
;;; (* (EXPT 2 -126.67 (ABS Xout))))

(defun reduce1 (x)
  (let ((pt *pi-table*)
        (e (exponent x))
        (u 0) (tiny 0))
    (when (<= e 1) ; Then (< (ABS x) 2)
      ;; Multiply by (+ (/ (aref pt 0) (expt 2 64)) (/ (aref pt (expt
      ;; 2 128)))).  These table entries are the first "digits" of (/
      ;; (* 2 PI)), with an error less than (expt 2 -130.22).
      (setf u (* (high x) (aref pt 1)))
      (setf tiny (ldb (byte 64 0) u))
      (setf (low x) (ash u -64))
      (setf u (* (high x) (aref pt 0)))
      (let ((u-low (ldb (byte 64 0) u)))
        (incf (low x) u-low)
        (setf (high x) (+ (ash u -64) (if (< (low x) u-low) 1 0))))
      ;; Let HI be the current value of (high x) and HI-IN the value
      ;; of (high x) when this function was entered.  Let LO be the
      ;; value of (low x).  Also, let PI-0 be the (AREF PT 0) and PI-1
      ;; (AREF pt 1). Then (+ HI (/ lo (expt 2 64)) (/ tiny (expt 2
      ;; 128))) is equal to (* HI-IN (+ (/ PI-0 (expt 2 64)) (/ PI-1
      ;; (expt 2 128).  Thus (< (abs (- (+ HI (/ lo (expt 2 64)) (/
      ;; tiny (expt 2 128))) (/ HI-IN (* 2 PI)))) (* HI-IN (expt 2
      ;; -130.22))).  Since X is normalized at input, (>= HI-IN (expt
      ;; 2 63)) and since (>= (AREF PT 0) (expt 2 61)), we have (>= HI
      ;; (expt 2 (- (+ 63 61) 64))) which is (expt 2 60).  Thus the
      ;; call to NORMALIZE below performs a left shift by at most 3
      ;; bits
      (setf e (exponent x))
      (normalize-custom-float-64 x)
      (decf e (exponent x))
      (unless (zerop e)
        (setf (low x) (logior (low x) (ash tiny (- 64 e)))))
      (return-from reduce1))
    ;; Now (<= 2 e 1024)
    ;;
    ;; FIXME: adapt this comment.  The upper 64-bit word X->hi
    ;; corresponds to hi/2^64*2^e, if multiplied by T[i]/2^((i+1)*64)
    ;; it yields hi*T[i]/2^128 * 2^(e-i*64).  If e-64i <= -128, it
    ;; contributes to less than 2^-128; if e-64i >= 128, it yields an
    ;; integer, which is 0 modulo 1.  We thus only consider the values
    ;; of i such that -127 <= e-64i <= 127, i.e., (-127+e)/64 <= i <=
    ;; (127+e)/64.  Up to 4 consecutive values of T[i] can contribute
    ;; (only 3 when e is a multiple of 64).
    (let ((i (if (< e 127) 0 (ceiling (- e 127) 64)))
          (c0 0) (c1 0) (c2 0) (c3 0) (c4 0))
      (declare (type (unsigned-byte 64) c0 c1 c2 c3 c4))
      (setf u (* (high x) (aref pt (+ i 3))))
      (setf c0 (ldb (byte 64 0) u))
      (setf c1 (ash u -64))
      (setf u (* (high x) (aref pt (+ i 2))))
      (incf c1 (ldb (byte 64 0) u))
      (setf c2 (+ (ash u -64) (if (< c1 (ldb (byte 64 0) u)) 1 0)))
      (setf u (* (high x) (aref pt (+ i 1))))
      (incf c2 (ldb (byte 64 0) u))
      (setf c3 (+ (ash u -64) (if (< c2 (ldb (byte 64 0) u)) 1 0)))
      (setf u (* (high x) (aref pt i)))
      (incf c3 (ldb (byte 64 0) u))
      (setf c4 (+ (ash u -64) (if (< c3 (ldb (byte 64 0) u)) 1 0)))
      ;; up to here, the ignored part hi*(T[i+4]+T[i+5]+...)  can
      ;; contribute by less than 2^64 in c[0], thus less than 1 in
      ;; c[1] */
      (let ((f (- e (* 64 i)))) ;; hi*T[i]/2^128 is multiplied by 2^f
        ;;  {c, 5} = hi*(T[i]+T[i+1]/2^64+T[i+2]/2^128+T[i+3]/2^192)
        ;;  now shift c[0..4] by f bits to the left
        (let ((tiny 0))
          (cond ((< f 64)
                 (setf (high x)
                       (logior (ldb (byte 64 0) (ash c4 f))
                               (ldb (byte 64 0) (ash c3 (- f 64)))))
                 (setf (low x)
                       (logior (ldb (byte 64 0) (ash c3 f))
                               (ldb (byte 64 0) (ash c2 (- f 64)))))
                 (setf tiny 
                       (logior (ldb (byte 64 0) (ash c2 f))
                               (ldb (byte 64 0) (ash c1 (- f 64))))))
                ((= f 64)
                 (setf (high x) c3)
                 (setf (low x) c2)
                 (setf tiny c1))
                ;; the ignored part was less than 1 in c[1],
                ;; thus less than 1 in tiny
                (t
                 ;; 65 <= f <= 127: this case can only occur
                 ;; when e >= 65
                 (let ((g (- f 64))) ;; 1 <= g <= 63
                   ;; we compute an extra term
                   (setf u (* (high x) (aref pt (+ i 4))))
                   (setf u (ash u -64))
                   (incf c0 u)
                   (incf c1 (if (< c0 u) 1 0))
                   (incf c2 (if (and (< c0 u) (zerop c1)) 1 0))
                   (incf c3 (if (and (< c0 u) (zerop c1) (zerop c2)) 1 0))
                   (incf c4 (if (and (< c0 u)
                                     (zerop c1)
                                     (zerop c2)
                                     (zerop c3))
                                1 0))
                   (setf (high x)
                         (logior (ldb (byte 64 0) (ash c3 g))
                                 (ldb (byte 64 0) (ash c2 (- 64 g)))))
                   (setf (low x)
                         (logior (ldb (byte 64 0) (ash c2 g))
                                 (ldb (byte 64 0) (ash c1 (- 64 g)))))
                   (setf tiny
                         (logior (ldb (byte 64 0) (ash c1 g))
                                 (ldb (byte 64 0) (ash c0 (- 64 g))))))))
          ;; the ignored part was less than 1 in c[0],
          ;; thus less than 1/2 in tiny
          ;; 
          ;; The approximation error between X/in(2pi) mod 1 and
          ;; X->hi/2^64 + X->lo/2^128 + tiny/2^192 is:
          ;; (a) the ignored part in tiny, which is less than ulp(tiny),
          ;; thus less than 1/2^192;
          ;; (b) the ignored terms hi*T[i+4] + ... or hi*T[i+5] + ...,
          ;; which accumulate to less than ulp(tiny) too, thus
          ;; less than 1/2^192.
          ;; Thus the approximation error is less than 2^-191 (absolute).
          (setf (exponent x) 0)
          (normalize-custom-float-64 X)
          ;; the worst case (for 2^25 <= x < 2^1024) is X->ex =
          ;; -61, attained for |x| = 0x1.6ac5b262ca1ffp+851
          (when (minusp (exponent x))
            ;;  put the upper -ex bits of tiny into low bits of lo
            (setf (low x) (logior (ash tiny (- -64 (exponent x)))))))))))
;;; Since X->ex >= -61, it means X >= 2^-62 before the normalization,
;;; thus the maximal absolute error of 2^-191 yields a relative error
;;; bounded by 2^-191/2^-62 = 2^-129.  There is an additional
;;; truncation error (for tiny) of at most 1 ulp of X->lo, thus at
;;; most 2^-127.  The relative error is thus bounded by 2^-126.67. */
