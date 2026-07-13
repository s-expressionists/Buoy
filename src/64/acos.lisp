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

(defun cr-acos (x)
  (let* ((xu (f-to-i x))
         ;; Contrary to the name, AX is not the absolute value of X,
         ;; but instead the bits of X shifted one position to the
         ;; left, so that the sign bit is not taken into account.
         (ax (ash xu 1))
         (1t 0d0)
         (z 0d0)
         (zl 0d0)
         (jd 0d0)
         (f0h 0d0)
         (f0l 0d0)
         (eps 0d0))
    ;; The reason comparisons are made using the bits of X rather than
    ;; the value of X is that X might represent infinity or NaN, and
    ;; then direct comparisons would fail.
    (if (> ax #x7fc0000000000000) ; |x| > 0.5
        (let* ((off10 #.(parse-c-literal "0x1.921fb54442d18p+1"))
               (off11 #.(parse-c-literal "0x1.1a62633145c07p-53"))
               (sign (ash ixu -63)))
          (setf f0h (if (zerop sign) 0d0 off10))
          (setf f0l (if (zerop sign) 0d0 off11))
          (when (> ax #x7fe0000000000000) ; |x| >= 1
            (cond ((= ax #x7fe0000000000000) ; |x| = 1
                   (return-from cr-acos (+ f0h f0l)))
                  ((> ax #xffe0000000000000) ; Nan
                   (return-from cr-acos (make-nan)))
                  ((zerop sign)
                   (error 'floating-point-overflow))
                  (t
                   (error 'floating-point-underflow))))
          ;; for x>0.5 we use range reduction for double angle formula
          ;; acos(x) = 2*asin((1-x)/2) and for x<-0.5 acos(x) = pi -
          ;; 2*asin((1-x)/2).

        

              
