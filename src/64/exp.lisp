(cl:in-package #:buoy-core-math-64)

(defun o-poly-dd (xh xl n table)
  (let* ((i (1- n))
         (ch (aref table i 0))
         (cl (aref table i 1)))
    (loop for j downfrom (1- i) to 0
          do (multiple-value-setq (ch cl) (multiply-dd xh xl ch cl))
             (let* ((th (+ ch (aref table i 0)))
                    (tl (+ (- (aref table i 0) th) ch)))
               (setf ch th)
               (incf cl (+ tl (aref table i 1)))))
    (values ch cl)))

(defun as-ldexp (x i)
  (let ((ix (quaviver:float-bits 'double-float x)))
    (incf ix (ash i 52))
    (quaviver:bits-float 'double-float ix)))

(defun cr-exp (x)
  (let* ((ix (quaviver:float-bits 'double-float x))
         (aix (logand ix (1- (ash 1 63)))))
    ;; exp(x) rounds to 1 to nearest for |x| <= 0x1p-54
    (when (< aix #x3c90000000000000)
        ;; |x| <= 0x1p-54
      (return-from cr-exp (+ 1d0 x)))
    (when (>= aix #x40862e42fefa39f0)
      (when (>= aix #x7ff0000000000000)
        (error 'floating-point-overflow))
      (when (= (ash ix -53) 0)
        ;; x >= 0x1.62e42fefa39fp+9
        (error 'floating-point-underflow))
      (when (>= aix #x40874910d52d3052)
        ;; x <= -0x1.74910d52d3052p+9
        (error 'floating-point-underflow)))
    (let* ((s 2^12/ln-2)
           (tt (round (* x s)))
           (jt tt)
           (i0 (logand (ash jt -6) #x3f))
           (i1 (logand jt #x3f))
           (ie (ash jt -12))
           (t0h (aref *t0-table* i0 1))
           (t0l (aref *t0-table* i0 0))
           (t1h (aref *t1-table* i1 1))
           (t1l (aref *t1-table* i1 0)))
      (multiple-value-bind (th tl)
          (multiply-dd t0h t0l t1h t1l)
        (let* ((l2h #.(parse-c-literal "0x1.62e42ffp-13"))
               (l2l #.(parse-c-literal "0x1.718432a1b0e26p-47"))
               ;; Use Cody-Waite argument reduction: since |x| < 745, we
               ;; have |t| < 2^23, thus since l2h is exactly
               ;; representable on 29 bits, l2h*t is exact.
               (dx (+ (- x (*  l2h tt)) (* l2l tt)))
               (dx2 (* dx dx))
               ;; |dx| < log(2)/2^13 (experimentally)
               ;;
               ;; 1 + x*(c0 + c1*x + ... + c3*x^3) is a degree-4
               ;; approximation of exp(x) on [-log(2)/2^13,log(2)/2^13]
               ;; with absolute error bounded by 2^-76.173 according to
               ;; Sollya
               (ch0 #.(parse-c-literal "0x1.0p+0"))
               (ch1 #.(parse-c-literal "0x1.0p-1"))
               (ch2 #.(parse-c-literal "0x1.55555557e54ffp-3"))
               (ch3 #.(parse-c-literal "0x1.55555553a12f4p-5"))
               (p (+ (+ ch0 (* dx ch1)) (* dx2 (+ ch2 (* dx ch3)))))
               (fh th)
               (tx (* th dx))
               (fl (+ tl (* tx p)))
               (eps 1.64d-19))
          (if (> ix #xc086232bdd7abcd2)
              ;; subnormal case: x < -0x1.6232bdd7abcd2p+9
              (progn
                (setf ix (ash (- 1 ie) 52))
                (multiple-value-bind (fh e)
                    (fast-two-sum (quaviver:bits-float 'double-float ix) fh)
                  (incf fl e)
                  (let ((ub (+ fh (+ fl eps)))
                        (lb (+ fh (- fl eps))))
                    (if (/= ub lb)
                        (as-exp-accurate x)
                        (setf fh (as-todenormal lb))))))
              (let ((ub (+ fh (+ fl eps)))
                    (lb (+ fh (- fl eps))))
                (if (/= ub lb)
                    (as-exp-accurate x)
                    (setf fh (as-ldexp lb ie)))))
          fh)))))
