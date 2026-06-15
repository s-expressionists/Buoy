(cl:in-package #:buoy-core-math-64)

;;; The function sinh(x) is approximated by a minimax polynomial for
;;; |x|<0.25. For other arguments the identity
;;; sinh(x)=(exp(|x|)-exp(-|x|))/2*copysign(1,x) is used. For |x|<5
;;; both exponents are calculated with slightly higher precision than
;;; double. For 5<|x|<36.736801 the exp(-|x|) is small and is
;;; calculated with double precision but exp(|x|) is calculated with
;;; higher than double precision. For 36.736801<|x|<710.47586
;;; exp(-|x|) becomes too small and only exp(|x|) is calculated.

(defun cr-sinh (x)
  (let* ((t0 *t0-table*)
         (t1 *t1-table*)
         (s +2^12/ln-2+)
         (ax (abs x))
         (v0 (fma ax s #.(parse-c-literal "0x1.8000002p+26")))
         (jtu (f-to-i v0))
         (vu jtu)
         (tt (- (ash 1 64) 2 (ash 1 26)))
         (vu (logand vu tt))
         (vf (i-to-f vu))
         (t1 (- vf #.(parse-c-literal "0x1.8p26")))
         (ixu (f-to-i ax))
         (aix ixu))
    (when (< aix #x3fd0000000000000) ; |x| < 0x1p-2
      (if (< aix #x3e57137449123ef7)
          ;; |x| < x0 = 0x1.7137449123ef7p-26
          (progn 
            ;; We have underflow exactly when 0 < |x| < 2^-1022: for
            ;; RNDU, sinh(2^-1022-2^-1074) would round to
            ;; 2^-1022-2^-1075 with unbounded exponent range
            (when (and (/= x 0d0) (< ax #.(parse-c-literal "0x1.0p-1022")))
              (error 'floating-point-underflow))
            ;; With p = c[0]*x^3 + c[1]*x^5 + c[2]*x^7 + c[3]*x^9 +
            ;; c[4]*x^11, q = x + p is a minimax approximation of
            ;; sinh(x) on [x0,1/4] such that |q - sinh(x)|/x^3 <
            ;; 2^-56.584 
            (let* ((c0 #.(parse-c-literal "0x1.5555555555555p-3"))
                   (c1 #.(parse-c-literal "0x1.111111111151ep-7"))
                   (c2 #.(parse-c-literal "0x1.a01a019d0c767p-13"))
                   (c3 #.(parse-c-literal "0x1.71de444a96e11p-19"))
                   (c4 #.(parse-c-literal "0x1.ae8465375242p-26"))
                   (x2 (* x x))
                   (x3 (* x2 x))
                   (x4 (* x2 x2))
                   (p (* x3
                         (+ (+ c0 (* x2 c1))
                            (* x4 (+ (+ c2 (* x2 c3))
                                     (* x4 c4))))))
                   (e (* x3 #.(parse-c-literal "0x1.cp-53")))
                   (lb (+ x (- p e)))
                   (ub (+ x (+ p e))))
              (return-from cr-sinh
                (if (= ub lb) lb (as-sinh-zero x)))))))
    (when (> aix #x408633ce8fb9f87) ; |x| >~ 710.47586
      (if (>= aix #x7ff0000000000000)
          (error 'floating-point-overflow)
          (return-from cr-sinh
            (* (copy-sign #.(parse-c-literal "0x1p1023") x) 2d0))))
    ;; now 0.25 <= |x| < 710.47586
    ;; this branch was checked exhaustively with/without FMA
    (let* ((il (ash (ash jtu 14) -40))
           (jl (- il))
           (i1 (logand il #x3f))
           (i0 (logand (ash il -6) #x3f))
           (ie (ash il -12))
           (ji (logand jl #x3f))
           (j0 (logand (ash jl -6) #x3f))
           (je (ash jl -12))
           (spu (ash (+ 1022 ie) 52))
           (spf (i-to-f spu))
           (smu (ash (+ 1022 je) 52))
           (smf (i-to-f smu))
           (t0h (aref t0 i0 1))
           (t0l (aref t0 i0 0))
           (t1h (aref t1 i1 1))
           (t1l (aref t1 i1 0))
           (th (* t0h t1h))
           (tl (+ (* t0h t1l) (* t1h t0l) (fma t0h t1h (- th))))
           (l2h +ln-2/2^12-high+)
           (l2l +ln-2/2^12-low+)
           (dx (+ (- ax (* l2h t1)) (* l2l t1)))
           (dx2 (* dx dx))
           (mx (- dx))
           
