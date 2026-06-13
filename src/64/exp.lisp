(cl:in-package #:buoy-core-math-64)

(defun o-poly-dd (xh xl n table)
  (let* ((i (1- n))
         (ch (aref table i 0))
         (cl (aref table i 1)))
    (loop for j downfrom (1- i) to 0
          do (multiple-value-setq (ch cl) (multiply-dd xh xl ch cl))
             (let* ((th (+ ch (aref table j 0)))
                    (tl (+ (- (aref table j 0) th) ch)))
               (setf ch th)
               (incf cl (+ tl (aref table j 1)))))
    (values ch cl)))

(defparameter *exp-poly-*
  (make-array
   '(7 2)
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (list (p "0x1.0p+0")
                  0d0)
           (list (p "0x1.0p-1")
                 (p "0x1.712f72ecec2cfp-99"))
           (list (p "0x1.5555555555555p-3")
                 (p "0x1.5555555554d07p-57"))
           (list (p "0x1.5555555555555p-5")
                 (p "0x1.55194d28275dap-59"))
           (list (p "0x1.1111111111111p-7")
                 (p "0x1.12faa0e1c0f7bp-63"))
           (list (p "0x1.6c16c16da6973p-10")
                 (p "-0x1.4ba45ab25d2a3p-64"))
           (list (p "0x1.a01a019eb7f31p-13")
                 (p "-0x1.9091d845ecd36p-67"))))))

(defun as-ldexp (x i)
  (let ((ix (quaviver:float-bits 'double-float x)))
    (incf ix (ash i 52))
    (quaviver:bits-float 'double-float ix)))

(defun as-exp-accurate (x)
  (flet ((f-to-i (x)
           (quaviver:float-bits 'double-float x))
         (i-to-f (i)
           (quaviver:bits-float 'double-float i)))
    (let* ((fx x)
           (ix (f-to-i fx)))
      (when (< (logand (ash ix -52) #x7ff) #x3c9)
        (return-from as-exp-accurate (+ 1d0 x)))
      (let* ((s +2^12/ln-2+)
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
          (let* ((l2h +ln-2/2^12-high+)
                 (l2l +ln-2/2^12-low+)
                 (l2ll +ln-2/2^12-low-low+)
                 ;; Use Cody-Waite argument reduction: since |x| < 745, we
                 ;; have |t| < 2^23, thus since l2h is exactly
                 ;; representable on 29 bits, l2h*t is exact.
                 (dx (- x (* l2h tt)))
                 (dxl (* l2l tt))
                 (dxll (+ (* l2ll tt) (fma l2l t (- dxl))))
                 (dxh (+ dx dxl)))
            (setf dxl (+ (- dx dxh) dxl dxll))
            (multiple-value-bind (fl fh)
                (o-poly-dd dxh dxl 7 *exp-poly-*)
              (multiple-value-setq (fh fl)
                (multiply-dd dxh dxl fh fl))
              (if (> ix #xc086232bdd7abcd2)
                  (progn ; x < -0x1.6232bdd7abcd2p+9
                    (setf ix (ash (- 1 ie) 52))
                    (setf fx (i-to-f ix))
                    (multiple-value-setq (fh fl)
                      (multiply-dd fh fl th tl))
                    (multiple-value-setq (fh fl)
                      (fast-sum th tl fh fl))
                    (let ((e 0d0))
                      (multiple-value-setq (fh e)
                        (fast-two-sum fx fh))
                      (incf fl e)
                      (setf fh (as-todenormal (+ fh fl)))))
                  (progn
                    (if (= th 1d0)
                        (progn
                          (let ((e 0d0))
                            (multiple-value-setq (fh e)
                              (fast-two-sum th fh))
                            (multiple-value-setq (fl e)
                              (fast-two-sum e fl))
                            (setf fx fl)
                            (setf ix (f-to-i fx))
                            (when (zerop (logand ix (1- (ash 1 52))))
                              (let* ((v (f-to-i e))
                                     (d (1+ (logxor (ash ix -63)
                                                    (ash (ash v -63) 1)))))
                                (incf ix d)
                                (setf fx (i-to-f ix))
                                (setf fl fx)))))
                        (progn (multiple-value-setq (fh fl)
                                 (multiply-dd fh fl th tl))
                               (multiple-value-setq (fh fl)
                                 (fast-sum th tl fh fl))))))
              (multiple-value-setq (fh fl)
                (fast-two-sum fh fl))
              (setf fx fl)
              (setf ix (f-to-i fx))
              (let ((d (logand (+ ix 2) (1- (ash 1 52)))))
                (when (<= d 2)
                  (setf fh (as-exp-database x fh)))
                (setf fh (as-ldexp fh ie)))
              fh)))))))

;;; The basic techniqe for argument reduction goes like this: You want
;;; to compute e^x.  You start by multiplying x by lb(e) so that e^x =
;;; 2^(x*lb(e)) which is the same as 2^(x/ln(2)).  Then you take the
;;; integer and the fractional part of x/ln(2), say i and f. so that
;;; you have 2^(i+f) which is 2^i * 2^f.  2^i is an integer, and you
;;; can just modify the exponent of 2^f in order to do the
;;; multiplication.  Now 2^f can be computed as is, or you can replace
;;; it with e^(f*ln(2)).  If the integer part was computed using
;;; floor, then, f is positive, so that the 0 <= f*ln(2) < 1. Or the
;;; integer part can be computed using round, so that -1/2 < f*ln(2) <
;;; 1/2.
;;;
;;; The technique used here splits x/ln(2) in more parts, say, a, b,
;;; c, and d, such that x/ln(2) = a + b*2^-6 + c*2^-12 + d, so that a
;;; is an integer, b and c are integers between 0 and 63 (i.e., 6 bit
;;; numbers), and d is the fraction which is then less than 2-12, so
;;; that e^x = 2^a * 2^(b*2^-6) * 2^(c * 2^-12) * e^(d*ln(2)).  The
;;; tables t0 and t1 are used to compute the two middle factors as
;;; double-doubles.  Here, a, b, and c are computed by multiplying x
;;; by 2^12/ln(2) and rounding the result to an integer that is called
;;; tt in this code.  Bits 5-0 of tt are then the value c above, and
;;; bits 11-6 are the value b.  The remaining upper bits are the value
;;; a.  Since ROUND is used, the value d is -2^13 < d < 2^13.  To find
;;; d,, or rather d*ln(2), tt is multiplied by ln(2)/2^12 and that
;;; value is subtracted from x. The value d*ln(x) is called dx in the
;;; code below.
;;;
;;; The variables l2h and l2l together, i.e. l2h-l2l (notice the
;;; difference and not the sum represent ln(2)/2^12.  These variables
;;; are derived in a special way so that l2h has exactly 29
;;; significant bits.  And since |x| < 745 (or else there would be a
;;; floating-point overflow), the result is that |tt| < 2^23.  When a
;;; 29-bit number is multiplied by a 23-bit number, the result has at
;;; most 52 bits, so that it fits in a double float.  We need to
;;; compute x - tt*(l2h-l2l) to get to the remainder, but we obviously
;;; can't add l2h and l2l first, so we expand this expression to
;;; ((x-tt*l2h) + tt* l2l.  That way, (x-tt*l2h) is small enough that
;;; tt*l2l can be added.

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
    (let* ((s +2^12/ln-2+)
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
        (let* ((l2h +ln-2/2^12-high+)
               (l2l +ln-2/2^12-low+)
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
