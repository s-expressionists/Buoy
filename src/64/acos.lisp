(cl:in-package #:buoy-core-math-64)

;;; Consider x as cos(phi) then sin(phi) is ch + cl = sqrt(1-x^2)
;;; Using angle rotation formula bring the argument close to zero
;;; where the asin Taylor expansion works well: acos(x) =
;;; asin(sqrt(1-x^2)) for x > 0 acos(x) = pi+asin(-sqrt(1-x^2)) for x
;;; < 0
(defun as-acos-refine (x phi)
  (let* ((s2 (* x x*))
         (dx2 (fma x x (- s2))))
    ;; s2 + dx2 = x²
    (multiple-value-bind (c2h c2l)
        (fast-two-sum 1d0 (- s2))
      (decf c2l dx2)
      (multiple-value-setq (c2h c2l)
        (fast-two-sum c2h c2l))
      ;; c2h+c2l approximates 1-x²
      (let* ((ch (sqrt c2h))
             ;; let eps = ch^2-c2h, then c2h + c2l = ch^2 + c2l - eps,
             ;;thus sqrt(c2h + c2l) = sqrt(ch^2*(1+(c2l-eps)/ch^2)) ~
             ;;ch*(1 + (c2l-eps)/ch^2/2) = ch + (c2l-eps)/ch/2
             (cl (* (- c2l (fma ch ch (- c2h))) (/ 0.5d0 ch)))
             ;; now ch+cl approximates sqrt(1-x²)
             (magic1 #.(parse-c-literal "0x1.921fb54442d18p+0"))
             (magic2 #.(parse-c-literal "0x1.45f306dc9c883p+4"))
             (jf (round (abs (* (- phi magic1) magic2)))))
             ;; jf = round(|phi-pi/2|*64/pi)
        ;; let y = acos(x) and assume y = pi/2 -/+ jf*pi/64 - delta,
        ;; with |delta| < pi/128,
        ;; where -/+ means - for x > 0, and + for x < 0:
        ;; delta = pi/2 -/+ jf*pi/64 - y thus
        ;; sin(delta) = sin(pi/2 -/+ jf*pi/64 - y)
        ;;            = cos(-/+jf*pi/64 - y)
        ;;            = cos(-/+jf*pi/64)*cos(y) + sin(-/+jf*pi/64)*sin(y)
        ;;            = cos(jf*pi/64)*x -/+ sin(jf*pi/64)*sqrt(1-x^2)
        ;;
        ;; 0 <= jf <= 32
        (let* ((tt *asin-polynomial-approximations*)
               (cch (aref tt (- 32 jf) 1))
               (ccl (aref tt (- 32 jf) 0))
               (ssh (aref tt jf 1))
               (ssl (aref tt jf 0))
               ;; Ch+Cl approximates cos(jf*pi/64), Sh+Sl approximates
               ;; sin(jf*pi/64) thus sin(delta) = (Ch+Cl)*x -/+
               ;; (Sh+Sl)*sqrt(1-x^2) ~ sgn(x) * [ (Ch+Cl)*|x| -
               ;; (Sh+Sl)*(ch+cl)] */
               (ax (abs x))
               (dsh (- ax ssh))
               (dsl (- ssl))
               (dch (- ch cch))
               (dcl (- cl ccl))
               ;; now |x| = Sh+Sl + dsh+dsl, ch+cl = Ch+Cl + dch+dcl
               ;; thus sin(delta) ~ sgn(x) * [ (Ch+Cl)*(Sh+Sl +
               ;; dsh+dsl) - (Sh+Sl)*(Ch+Cl + dch+dcl)] ~ sgn(x) *
               ;; [(Ch+Cl)*(dsh+dsl) - (Sh+Sl)*(dch+dcl)].  Since
               ;; |delta| < pi/128 and y = pi/2 -/+ jf*pi/64 - delta,
               ;; |dsh|, |dch| < pi/128 < 0.0246
               (magic #.(parse-c-literal "0x1.8p4"))
               ;; Remark: we could reduce magic to 0x1.8p-5, then Cs -
               ;; Sc below would still be exact, but this would add
               ;; one exceptional case (x=-0x1.52f06359672cdp-2) and
               ;; save one, thus there is no benefit. */
               (sc (- (fma ssh dch magic) magic))
               (dsc (fma ssh dch (- sc)))
               ;; Sc + dSc approximates Sh*dch, with Sc multiple of
               ;; 2^-56 and |Sc| < 2^-5
               (cs (- (fma cch dsh magic) magic))
               (dcs (fma cch dsh (- cs)))
               ;; Cs+dCs approximates Ch*dsh, with Cs multiple of
               ;; 2^-56 and |Cs| < 2^-5
               (v (- ccs ssc))
               ;; v is exact since |ccs - ssc| is a multiple of 2⁵⁶
               ;; and < 2⁻⁴.
               ;;
               ;; v approximates cch · dsh - ssh · dch
               (dv (- (+ (* cch dsl) (* ccl dsh))
                      (+ (* ssh dcl) (* ssl dch))
                      (- dsc dcs))))
               ;; v+dv approximates (Ch+Cl)*(dsh+dsl) -
               ;; (Sh+Sl)*(dch+dcl) thus approximates by sgn(x) *
               ;; sin(delta)
          (multiple-value-setq (v dv)
            (fast-two-sum v dv))
          (let* ((sgn (copysign 1d0 x))
                 (jt (- 32 (* jf sgn))))
            ;; pi/2 -/+ jf*pi/64 = jt*pi/64 thus y = jt*pi/64 - delta
            ;; with 0 <= jt <= 64

(defun acos-final (x eps tt jd z zl f0h f0l)
  (let* ((j (round jd))
         (t2 (* tt tt))
         (d (asin-polynomial-approximation tt t2 j))
         (p *asin-polynomial-approximations*)
         (fh (aref p j 0))
         (fl (+ (aref p j 1) d)))
    (multiple-value-setq (fh fl)
      (multiply-dd z zl fh fl))
    (multiple-value-setq (fh fl)
      (fast-sum f0h f0l fh fl))
    (let ((lb (+ fh (- fl eps)))
          (ub (+ fh (+ fl eps))))
      (if (/= lb ub)
          (as-acos-refine x lb)
          lb))))

(defun acos-|x|<2^-15 (x ax f0h f0l)
  (let* ((c #.(parse-c-literal "-0x1.5555555555555p-3"))
         ;; Avoid a spurious underflow for |x| <= x0 :=
         ;; 0x1.cb3b3869747f4p-55; moreover for |x| <= x0
         ;; we always have lb=ub, thus the accurate path is
         ;; never called.
         (v (if (<= ax #x791967670d2e8fe8)
                0
                (* (* x x) (* c x)))))
    (multiple-value-bind (h w)
        (fast-two-sum f0h (- x))
      (let* ((l (+ v (+ w f0l)))
             (eps1 #.(parse-c-literal "0x1.34p-79"))
             (lb (+ h (- l eps1)))
             (ub (+ h (+ l eps1))))
        (if (/= lb ub)
            (as-acos-refine x lb)
            lb)))))

(defun acos--1<=x<=1 (x)
  (let* (;; Contrary to the name, AX is not the absolute value of X,
         ;; but instead the bits of X shifted one position to the
         ;; left, so that the sign bit is not taken into account.
         (ax (ash (f-to-i x) 1))
         (abs-x (abs x)))
    (cond ((= x 1d0)
           (return-from acos--1<=x<=1 0d0))
          ((= x -1d0)
           (let ((off0 #.(parse-c-literal "0x1.921fb54442d18p+1"))
                 (off1 #.(parse-c-literal "0x1.1a62633145c07p-53")))
             (return-from acos--1<=x<=1 (+ off0 off1))))
          ((> abs-x 0.5d0)
           ;; for x>0.5 we use range reduction acos(x) = 2 ·
           ;; asin(√((1-x)/2)) and for for x<-0.5 acos(x) = π - 2 ·
           ;; asin(√((1+x)/2)).
           (let* ((1t (- 2d0 (* 2d0 abs-x)))
                  (jd (round (* 1t 32d0)))
                  (z (copy-sign (sqrt 1t) x))
                  (zl (* (fma z z (- 1t)) (* (/ -0.5d0 1t) z))))
             (setf 1t (- (* 0.25d0 1t)
                         (* jd #.(parse-c-literal "0x1.0p-7"))))
             ;; fails with 0x1.8bp-52 for x=-0x1.3e827a2cd6d51p-1 (no FMA)
             (let ((eps (+ (* (abs (* z 1t))
                              #.(parse-c-literal "0x1.8cp-52"))
                           #.(parse-c-literal "0x1.0p-105"))))
               (acos-final x eps 1t jd z zl 0d0 0d0))))
          (t
           (let ((f0h #.(parse-c-literal "0x1.921fb54442d18p+0"))
                 (f0l #.(parse-c-literal " 0x1.1a62633145c07p-54")))
             (when (< abs-x #.(parse-c-literal "0x1.0p-15"))
               (return-from acos--1<=x<=1
                 (acos-|x|<2^-15 x ax f0h f0l)))
             ;; for 2^-15 <= |x| <= 0.5 we use acos(x) = pi/2 - asin(x)
             ;; so the argument range for asin is the same for both
             ;; branches to reuse the lookup tables.
             (let* ((jd (round (* (* x x) #.(parse-c-literal "0x1.0p7"))))
                    (z (- x))
                    (1t (fma x x (* #.(parse-c-literal "0x-1.0p-7") jd)))
                    ;; eps < 0 for x > 0, but the rounding test is
                    ;; still correct
                    ;;
                    ;; for |x| < 2^-4 (case j=0), fails with
                    ;; 0x1.d3p-53 and x=0x1.7cb54339263fbp-12; for
                    ;; 2^-4 <= |x| < 0.5, fails with 0x1.80p-52 and
                    ;; x=-0x1.fda6fee396f8p-2 (no FMA, rndz)
                    (eps (* (* z 1t) #.(parse-c-literal "0x1.81p-52"))))
               (acos-final x eps 1t jd z 0d0 f0h f0l)))))))

(defun acos-infinity-or-nan (x)
  (declare (ignore x))
  (error "Infinity or NaN supplied to ACOS"))

(defun cr-acos (x)
  (cond ((infinity-or-nan-p x)
         (acos-infinity-or-nan x))
        ;; Now it is safe to use floating-point comparisons.
        ((or (> x 1d0) (< x -1d0))
         (error 'type-error
                :datum x
                :expected-type '(dfloat -1d0 1d0)))
        (t
         (acos--1<=x<=1 x))))
