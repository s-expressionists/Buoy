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

(defun acos--1<=x<=1 (x)
  (let* ((xu (f-to-i x))
         ;; Contrary to the name, AX is not the absolute value of X,
         ;; but instead the bits of X shifted one position to the
         ;; left, so that the sign bit is not taken into account.
         (ax (ash xu 1))
         (abs-x (abs x))
         (1t 0d0)
         (z 0d0)
         (zl 0d0)
         (jd 0d0)
         (f0h 0d0)
         (f0l 0d0)
         (eps 0d0))
    (cond ((= x 1d0)
           (return-from acos--1<=x<=1 0d0))
          ((= x -1d0)
           (let ((off0 #.(parse-c-literal "0x1.921fb54442d18p+1"))
                 (off1 #.(parse-c-literal "0x1.1a62633145c07p-53")))
             (return-from acos--1<=x<=1 (+ off0 off1))))
          ((> absx 0.5d0)
           ;; for x>0.5 we use range reduction acos(x) = 2 ·
           ;; asin(√((1-x)/2)) and for for x<-0.5 acos(x) = π - 2 ·
           ;; asin(√((1+x)/2)).
           (setf 1t (- 2d0 (* 2d0 abs-x)))
           (setf jd (round (* 1t 32d0)))
           (setf z (copy-sign (sqrt 1t) x))
           (setf zl (* (fma z z (- 1t)) (* (/ -0.5d0 1t) z)))
           (setf 1t (- (* 0.25d0 1t) (* jd #.(parse-c-literal "1.0p-7"))))
           ;; fails with 0x1.8bp-52 for x=-0x1.3e827a2cd6d51p-1 (no FMA)
           (setf eps (+ (* (abs (* z 1t))
                           #.(parse-c-literal "0x1.8cp-52"))
                        #.(parse-c-literal "0x1.0p-105"))))
          (t
           (setf f0h #.(parse-c-literal "0x1.921fb54442d18p+0"))
           (setf f0l #.(parse-c-literal " 0x1.1a62633145c07p-54"))
           (when (< abs-x #.(parse-c-literal "1.0p-15"))
             



(defun acos-infinity-or-nan (x)
  (error "Infinity or NaN supplied to ACOS"))

(defun cr-acos (x)
  (cond ((infinity-or-nan-p x)
         (acos-infinity-or-nan x))
        ;; Now it is safe to use floating-point comparisons.
        ((or (> x 1d0) (< x -1d0))
         (error 'type-error
                :datum x
                :expected-type '(dfloat -1d0 1d0)))
        (acos--1<=x<=1 x)))
