(cl:in-package #:buoy-core-math-64)

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
