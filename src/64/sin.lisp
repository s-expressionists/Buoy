(cl:in-package #:buoy-core-math-64)

;;; Return three values, the error, the high double-float and the low
;;; double-float.
(defun sin-fast (x)
  (declare (type double-float high low x))
  (let ((negative (minusp x))
        (is-sin  1)
        (absolute-x (abs x))
        (table *sine-cosine-table*))
    ;; now x > 0x1.7137449123ef6p-26
    (multiple-value-bind (i err1 high low)
        (reduce-fast absolute-x)
      ;; err1 is an absolute bound for | i/2^11 + h + l -
      ;; frac(x/(2pi)) |: | i/2^11 + h + l - frac(x/(2pi)) | < err1
      ;;
      ;; if i >= 2^10: 1/2 <= frac(x/(2pi)) < 1 thus pi <= x <= 2pi we
      ;; use sin(pi+x) = -sin(x)
      (setf is-sin (logxor is-sin (ash i -9)))
      (setf i (logand i #x1ff))
      ;; | i/2^11 + h + l - frac(x/(2pi)) | mod 1/4 < err1
      ;;
      ;; now 0 <= i < 2^9
      ;; if i >= 2^8: 1/8 <= frac(x/(2pi)) < 1/4
      ;; we use sin(pi/2-x) = cos(x)
      (unless (zerop (logand i #x100))
        ;; case pi/4 <= x_red <= pi/2
        (setf is-sin (if (zerop if-sin) 1 0))
        (setf i (- #x1ff i))
        ;; 0x1p-11 - h is exact below: indeed, reduce_fast first
        ;; computes a first value of h (say h0, with 0 <= h0 < 1),
        ;; then i = floor(h0*2^11) and h1 = h0 - 2^11*i with 0 <= h1 <
        ;; 2^-11.  If i >= 2^8 here, this implies h0 >= 1/2^3, thus
        ;; ulp(h0) >= 2^-55: h0 and h1 are integer multiples of 2^-55.
        ;; Thus h1 = k*2^-55 with 0 <= k < 2^44 (since 0 <= h1 <
        ;; 2^-11).  Then 0x1p-11 - h = (2^44-k)*2^-55 is exactly
        ;; representable.  We can have a huge cancellation in 0x1p-11
        ;; - h, for example for x = 0x1.61a3db8c8d129p+1023 where we
        ;; have before this operation h = 0x1.ffffffffff8p-12, and h =
        ;; 0x1p-53 afterwards. But this does not hurt since we bound
        ;; the absolute error and not the relative error at the
        ;; end. */
        (setf high (- #.(parse-c-literal "0x1p-11") h))
        (setf low (- low)))
      ;; Now 0 <= i < 256 and 0 <= h+l < 2^-11 with | i/2^11 + h + l -
      ;; frac(x/(2pi)) | cmod 1/4 < err1 If is_sin=1, sin |x| = sin2pi
      ;; (R + err1); if is_sin=0, sin |x| = cos2pi (R + err1).  In
      ;; both cases R = i/2^11 + h + l, 0 <= R < 1/4.
      (let ((sh 0d0)
            (sl 0d0)
            (ch 0d0)
            (cl 0d0))
        ;; since the SC[] table evaluates at i/2^11 + SC[i][0] and not
        ;; at i/2^11, we must subtract SC[i][0] from h+l
        ;; 
        ;; Here h = k*2^-55 with 0 <= k < 2^44, and SC[i][0] is an
        ;; integer multiple of 2^-62, with |SC[i][0]| < 2^-24, thus
        ;; SC[i][0] = m*2^-62 with |m| < 2^38. It follows h-SC[i][0] =
        ;; (k*2^7 + m)*2^-62 with 2^51 - 2^38 < k*2^7 + m < 2^51 +
        ;; 2^38, thus h-SC[i][0] is exact.  Now |h| < 2^-11 +
        ;; 2^-24. */
        (decf high (aref table i 0))
        ;; now -2^-24 < h < 2^-11+2^-24
        ;; from reduce_fast() we have |l| < 2^-52.36
        (multiple-value-bind (uh ul)
            (a-multiply high high)
          (setf ul (fma (* high high) low ul))
          ;; uh+ul approximates (h+l)^2
          (multiple-value-bind (sh sl)
              (eval-fast-polynomial-sine high low uh ul)
            ;; the absolute error of evalPSfast() is less than
            ;; 2^-77.09 from routine evalPSfast() in sin.sage: | sh +
            ;; sh - sin2pi(h+l) | < 2^-77.09
            (multiple-value-bind (ch cl)
                (eval-fast-polynomial-cosine uh ul)
              ;; the relative error of evalPCfast() is less than
              ;; 2^-69.96 from routine evalPCfast(rel=true) in
              ;; sin.sage: | ch + cl - cos2pi(h+l) | < 2^-69.96 * |ch
              ;; + cl| 
              (let ((err 0d0)
                    (sgn0 1d0)
                    (sgn1 -1d0))
                (if (not (zerop is-sin))
                    (multiple-value-bind (sh sl)
                        (s-multiply (* (if (zerop neg) sgn0 sgn1)
                                       (aref table i 2) sh sl))
                      (multiple-value-bind (ch cl)
                          (s-multiply (* (if (zerop neg) sgn0 sgn1)
                                         (aref table i 1) ch cl))
                        (multiple-value-bind (h l)
                            (fast-two-sum ch sh)
                          (incf low (+ sl cl))
                          ;; absolute error bounded by 2^-68.588 from
                          ;; global_error(is_sin=true,rel=false) in
                          ;; sin.sage: | h + l - sin2pi (R) | <
                          ;; 2^-68.588 thus: | h + l - sin |x| | <
                          ;; 2^-68.588 + | sin2pi (R) - sin |x| |
                          ;; < 2^-68.588 + err1 */
                          (setf err #.(parse-c-literal "0x1.81p-69")))))
                    (multiple-value-bind (ch cl)
                        (s-multiply (* (if (zerop neg) sgn0 sgn1)
                                       (aref table i 2) ch cl))
                      (multiple-value-bind (sh sl)
                          (s-multiply (* (if (zerop neg) sgn0 sgn1)
                                         (aref table i 1) sh sl))
                        (multiple-value-bind (h l)
                            (fast-two-sum ch (- sh))
                          (incf low (- cl sl))
                          ;; absolute error bounded by 2^-68.414 from
                          ;; global_error(is_sin=false,rel=false) in
                          ;; sin.sage: | h + l - cos2pi (R) | <
                          ;; 2^-68.414 thus: | h + l - sin |x| | <
                          ;; 2^-68.414 + | cos2pi (R) - sin |x| | <
                          ;; 2^-68.414 + err1 */
                          (setf err
                                #.(parse-c-literal "0x1.81p-69")))))))))))
      (values (+ err err1) high low))))
