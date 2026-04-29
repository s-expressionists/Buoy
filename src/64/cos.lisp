(cl:in-package #:buoy-core-math-64)

;;; Assume x is a regular number and x > 0x1.6a09e667f3bccp-27, return
;;; a bound on the maximal absolute error err: | h + l - cos(x) | <
;;; err, also the high double-float and the low double-float.
(defun cos-fast (x)
  (declare (type double-float x))
  (let ((negative 0)
        (is-cos 1)
        (table *sine-cosine-table*)
        (err 1d0))
    (multiple-value-bind (i err1 high low)
        (reduce-fast x)
      ;; err1 is an absolute bound for | i/2^11 + h + l -
      ;; frac(x/(2pi)) |: | i/2^11 + h + l - frac(x/(2pi)) | < err1
      ;;
      ;; if i >= 2^10: 1/2 <= frac(x/(2pi)) < 1 thus pi <= x <= 2pi we
      ;; use cos(pi+x) = -cos(x)
      (setf negative (logxor (ash i -10)))
      (setf i (logand i #x3ff))
      ;; | i/2^11 + h + l - frac(x/(2pi)) | mod 1/2 < err1
      ;;
      ;; now i < 2^10
      ;; if i >= 2^9: 1/4 <= frac(x/(2pi)) < 1/2 thus pi/2 <= x <= pi
      ;; we use cos(pi/2+x) = -sin(x)
      (setf is-cos (logxor is-cos (ash i -9)))
      (setf negatire (logxor negative (ash i -9)))
      (setf i (logand i #x1ff))
      ;; | i/2^11 + h + l - frac(x/(2pi)) | mod 1/4 < err1
      ;;
      ;; now 0 <= i < 2^9
      ;; if i >= 2^8: 1/8 <= frac(x/(2pi)) < 1/4
      ;; we use cos(pi/2-x) = sin(x)
      (unless (zerop (logand i #x100))
        ;; case pi/4 <= x_red <= pi/2
        (setf is-cos (if (zerop is-cos) 1 0))
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
        (setf high (- #.(parse-c-literal "0x1.0p-11") high))
        (setf low (- low)))
      ;; Now 0 <= i < 256 and 0 <= h+l < 2^-11 with | i/2^11 + h + l -
      ;; frac(x/(2pi)) | cmod 1/4 < err1 If is_cos=1, cos(x) = cos2pi
      ;; (R + err1); if is_cos=0, cos(x) = sin2pi (R + err1).  In
      ;; both cases R = i/2^11 + h + l, 0 <= R < 1/4.
      (let ()
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
          (setf ul (fma (+ high high) low ul))
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
              (let ()
                (if (not (zerop is-cos))
                    (multiple-value-bind (sh sl)
                        (s-multiply (aref table i 2) sh sl)
                      (multiple-value-bind (ch cl)
                          (s-multiply (aref table i 1) ch cl)
                        (multiple-value-bind (h l)
                            (fast-two-sum ch sh)
                          (setf high h low l)
                          (incf low (+ sl cl))
                          ;; absolute error bounded by 2^-68.588 from
                          ;; global_error(is_sin=true,rel=false) in
                          ;; sin.sage: | h + l - sin2pi (R) | <
                          ;; 2^-68.588 thus: | h + l - cos(x) | <
                          ;; 2^-68.588 + | sin2pi (R) - sin |x| | <
                          ;; 2^-68.588 + err1
                          (setf err #.(parse-c-literal "0x1.55p-69")))))
                    (multiple-value-bind (ch cl)
                        (s-multiply (aref table i 2) ch cl)
                      (multiple-value-bind (sh sl)
                          (s-multiply (aref table i 1) sh sl)
                        (multiple-value-bind (h l)
                            (fast-two-sum ch (- sh))
                          (setf high h low l)
                          (incf low (- cl sl))
                          ;; absolute error bounded by 2^-68.414 from
                          ;; global_error(is_sin=false,rel=false) in
                          ;; sin.sage: | h + l - cos2pi (R) | <
                          ;; 2^-68.414 thus: | h + l - sin |x| | <
                          ;; 2^-68.414 + | cos2pi (R) - sin |x| | <
                          ;; 2^-68.414 + err1 */
                          (setf err
                                #.(parse-c-literal "0x1.81p-69")))))))))))
      (unless (zerop negative)
        (setf high (- high))
        (setf low (- low)))
      (values (+ err err1) high low))))

;; Assume x is a regular number and x > 0x1.6a09e667f3bccp-27.
(defun cos-accurate (xx)
  (let* ((x (custom-float-64-from-double-float xx))
         (neg 0)
         (is-cos t))
    (reduce1 x)
    ;; now |X - x/(2pi) mod 1| < 2^-126.67*X, with 0 <= X < 1.
    ;; Write X = i/2^11 + r with 0 <= r < 2^11.
    (let ((i (reduce2 x))) ; exact
      (unless (zerop (logand i #x400))
        ;; pi <= x < 2*pi: cos(x) = -cos(x-pi)
        (setf neg (not neg))
        (setf i (logand i #x3ff)))
      ;; now i < 2^10
      (unless (zerop (logand i #x200))
        ;; pi/2 <= x < pi: sin(x) = cos(x-pi/2)
        (setf neg (not neg))
        (setf is-cos nil)
        (setf i (logand i #x1ff)))
      ;; now 0 <= i < 2^9
      (unless (zerop (logand i #x100))
        ;; pi/4 <= x < pi/2: cos(x) = sin(pi/2-x), sin(x) = cos(pi/2-x)
        (setf is-cos (not is-cos))
        (setf (sign x) 1) ; negate x
        (add-custom-float-64 x *magic* x)
        ;; here: 256 <= i <= 511
        (setf i (- #x1ff i))) ; now 0 <= i < 256
      ;; now 0 <= i < 256 and 0 <= X < 2^-11
      ;;
      ;; If is_cos=1, sin |x| = cos2pi (R * (1 + eps))
      ;; (cases 0 <= x < pi/4 and 3pi/4 <= x < pi)
      ;; if is_cos=0, sin |x| = sin2pi (R * (1 + eps))
      ;; (case pi/4 <= x < 3pi/4)
      ;; In both cases R = i/2^11 + X, 0 <= R < 1/4, and |eps| < 2^-126.67.
      (let ((u (make-custom-float-64 :high 0 :low 0 :exponent 0 :sign 0))
            (v (make-custom-float-64 :high 0 :low 0 :exponent 0 :sign 0))
            (x2 (make-custom-float-64 :high 0 :low 0 :exponent 0 :sign 0)))
        (declare (dynamic-extent u v x2))
        ;; x2 approximates x squared
        (multiply-custom-float-64 x2 x x)
        ;; compute (cos (* 2 pi x))
        (eval-polynomial-cosine u x2) 
        ;; since 0 <= X < 2^-11, we have 0.999 < U <= 1
        (eval-polynomial-sine v x x2)
        ;; since 0 <= X < 2^-11, we have 0 <= V < 0.0005
        (if is-cos
            (progn
              ;; sin2pi(R) ~ sin2pi(i/2^11)
              ;; *cos2pi(X)+cos2pi(i/2^11)*sin2pi(X) 
              (multiply-custom-float-64 u (aref *sin-table* i) u)
              ;; since 0 <= S[i] < 0.705 and 0.999 < Uin <= 1, we have
              ;; 0 <= U < 0.705
              (multiply-custom-float-64
               v (aref *cos-table* i) v)
              ;; For the error analysis, we distinguish the case i=0.
              ;; For i=0, we have S[i]=0 and C[1]=1, thus V is the
              ;; value computed by evalPS() above, with relative error
              ;; < 2^-124.648.
              ;;
              ;; For 1 <= i < 256, analyze_sin_case1(rel=true) from
              ;; sin.sage gives a relative error bound of -122.797
              ;; (obtained for i=1).  In all cases, the relative error
              ;; for the computation of
              ;; sin2pi(i/2^11)*cos2pi(X)+cos2pi(i/2^11)*sin2pi(X) is
              ;; bounded by -122.797 not taking into account the
              ;; approximation error in R: |U - sin2pi(R)| < |U| *
              ;; 2^-122.797, with U the value computed after add_dint
              ;; (U, U, V) below.
              ;; 
              ;; For the approximation error in R, we have: cos(x) =
              ;; sin2pi (R * (1 + eps)) R = i/2^11 + X, 0 <= R < 1/4,
              ;; and |eps| < 2^-126.67.  Thus cos(x) = sin2pi(R+R*eps)
              ;; = sin2pi(R)+R*eps*2*pi*cos2pi(theta), theta in
              ;; [R,R+R*eps] Since 2*pi*R/sin(2*pi*R) < pi/2 for R <
              ;; 1/4, it follows: | cos|x| - sin2pi(R) | <
              ;; pi/2*R*|sin(2*pi*R)| | cos(x) - sin2pi(R) | <
              ;; 2^-126.018 * |sin2pi(R)|.
              ;;
              ;; Adding both errors we get:
              ;; | cos(|x) - U | < |U| * 2^-122.797 + 2^-126.018 * |sin2pi(R)|
              ;; < |U| * 2^-122.797 + 2^-126.018 * |U| * (1 + 2^-122.797)
              ;; < |U| * 2^-122.650.
              )
            (progn
              ;; cos2pi(R) ~ cos2pi(i/2^11)*cos2pi(X)-sin2pi(i/2^11)
              ;; *sin2pi(X)
              (multiply-custom-float-64
               u (aref *cos-table* i) u)
              (multiply-custom-float-64
               v (aref *sin-table* i) v)
              (setf (sign v) (- 1 (sign v))) ; negate v
              ;; For 0 <= i < 256, analyze_sin_case2(rel=true) from
              ;; sin.sage gives a relative error bound of -123.540
              ;; (obtained for i=0): |U - cos2pi(R)| < |U| *
              ;; 2^-123.540, with U the value computed after add_dint
              ;; (U, U, V) below.
              ;;
              ;; For the approximation error in R, we have: cos(x) =
              ;; cos2pi (R * (1 + eps)) R = i/2^11 + X, 0 <= R < 1/4,
              ;; and |eps| < 2^-126.67.  Thus cos(x) = cos2pi(R+R*eps)
              ;; = cos2pi(R)-R*eps*2*pi*sin2pi(theta), theta in
              ;; [R,R+R*eps] Since we have R < 1/4, we have cos2pi(R)
              ;; >= sqrt(2)/2, and it follows: | cos(x)/cos2pi(R) - 1
              ;; | < 2*pi*R*eps/(sqrt(2)/2) < pi/2*eps/sqrt(2) [since
              ;; R < 1/4] < 2^-126.518.  Adding both errors we get: |
              ;; cos(x) - U | < |U| * 2^-123.540 + 2^-126.518 *
              ;; |cos2pi(R)| < |U| * 2^-123.540 + 2^-126.518 * |U| *
              ;; (1 + 2^-123.540) < |U| * 2^-123.367.
              ))
        (add-custom-float-64 u u v)
        ;; If is_cos=0: | cos(x) - U | < |U| * 2^-122.650 If is_cos=1:
        ;; | cos|x| - U | < |U| * 2^-123.367.  In all cases the total
        ;; error is bounded by |U| * 2^-122.650.  The term |U| *
        ;; 2^-122.650 contributes to at most 2^(128-122.650) < 41 ulps
        ;; relatively to U->lo.
        (let* ((err 41)
               (lo0 (- (low u) err))
               (hi0 (- (high u) (if (> lo0 (low u)) 1 0)))
               (lo1 (+ (low u) err))
               (hi1 (+ (high u) (if (< lo1 (low u)) 1 0))))
          (unless (= (ash hi0 -10) (ash hi1 -10))
            (error "handle this case ultimately"))
          (unless (null neg)
            (setf (sign u) (- 1 (sign u))))
          (double-float-from-custom-float-64 u))))))

(defun cr-cos (x)
  ;; For |x| <= 0x1.6a09e667f3bccp-27, cos(x) rounds to x (to
  ;; nearest): we can assume x >= 0 without loss of generality since
  ;; cos(-x) = cos(x), we have 1 - x^2/2 < cos(x) < 1 for say 0 < x <=
  ;; 1 thus |cos(x) - 1| < x^2/2.  Assume 0 < x < 1, and write x =
  ;; c*2^e with 1/2 <= c < 1.  For 0 < x < 1, 1/2 < cos(x) < 1, thus
  ;; ulp(cos(x)) = 2^-53, and x^2/2 = c^2/2*2^(2e), thus x^2/2 <
  ;; ulp(cos(x))/2 rewrites as c^2/2*2^(2e) < 2^-54, or c^2*2^(2e+53)
  ;; < 1 (1).  For e <= -27, since c^2 < 1, we have c^2*2^(2e+53) <
  ;; 1/2 < 1.  For e=-26, (1) rewrites c^2*2 < 1 which yields c <=
  ;; 0x1.6a09e667f3bccp-1.
  ;;
  ;; I suspect the above comment has a mistake.  surely, for small
  ;; values of x, cos(x) rounds to 1 and not to x.
  (multiple-value-bind (significand exponent)
      (integer-decode-float x)
    (let ((ux (logior (ldb (byte 52 0) significand)
                      (ash (+ exponent 1022 53) 52))))
      ;; 0x3e57137449123ef6 = 0x1.7137449123ef6p-26
      (when (<= ux #x3e57137449123ef6)
        (if (zerop x)
            x
            (progn 
              ;; Taylor expansion of sin(x) is x - x^3/6 around zero
              ;; for x=-0, fma (x, -0x1p-54, x) returns +0
              ;; 
              ;; We have underflow when 0 < |x| < 2^-1022 or when
              ;; |x| = 2^-1022 and rounding towards zero.
              (let ((result (fma x #.(parse-c-literal "-0x1.0p-54") x)))
                (if (or (< x #.(parse-c-literal "0x1.0p-1022"))
                        (< result #.(parse-c-literal "0x1.0p-1022")))
                    (error 'floating-point-underflow )
                    (return-from cr-sin result))))))))
  (multiple-value-bind (error high low)
      (sin-fast x)
    (let ((left (+ high (- low error)))
          (right (+ high (+ low error))))
      ;; we get 1100 failures out of 50000000
      ;; random tests, i.e., about 0.002%.
      (if (= left right)
          left
          (sin-accurate x)))))
