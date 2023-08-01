(cl:in-package #:buoy)

;; The cosine and sine functions, for single floats.  Adapted from RLIBM.

;; Copyright (c) 2022 Sehyeok Park, Mridul Aanjaneya, and Santosh
;; Nagarakatte, Rutgers Architecture and Programming Languages (RAPL)
;; Group
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; little-endian coefficients
;; for even, first coeff is constant (x^0); for odd, first coeff is for x^1
(defmacro even-polynomial (type x &rest coeffs)
  (let ((xs (gensym))
        (x2 (gensym)))
    `(let* ((,xs (the ,type ,x))
            (,x2 (* ,xs ,xs)))
       ,(loop for coeff in (mapcar (lambda (y) `(the ,type ,y)) (reverse coeffs))
              for form = coeff then `(fma-double ,coeff ,x2 ,form)
              finally (return form)))))

(defmacro odd-polynomial (type x &rest coeffs)
  (let ((xs (gensym)))
    `(let* ((,xs (the ,type ,x)))
       (* ,xs (even-polynomial ,type ,xs ,@coeffs)))))
  

(declaim (ftype (function (single-float) (single-float -1s0 1s0)) cos-single sin-single))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pi/256+ (u2d #x3f8921fb54442d18))
  (defconstant +range-reduce-large-threshold+ #x4a000000) ; 2097152; when >= this, need a 256-bit pi reciprocal approximation
  (defconstant +cos-one-threshold+ #x39800001)            ; ~2.44e-4; cos <this is just 1s0
  (defconstant +cos-simple-threshold+ #x3c490fdb)         ; ~0.0123; cos <this can be approximated with a low-degree even polynomial
  (unless (boundp '+cos-simple-polynomial+)
    (defconstant +cos-simple-polynomial+ (mapcar #'u2d '(#x3ff0000000000002 #xbfe00000000a7c61  #x3fa5558e8686159f))))
  (unless (boundp '+cos-hard-to-round-cases+)
    (defconstant +cos-hard-to-round-cases+
      '((#x5922aa80 . #x3f08aebf)
        (#x6a127977 . #x3ed7ae35)
        (#x7908cd73 . #x3f798bb5)
        (#x7a38ab34 . #x3f7b3195))))
  (unless (boundp '+cos-sine-polynomial+)
    (defconstant +cos-sine-polynomial+ (mapcar #'u2d '(#x3ff0000000000001 #xbfc55555555d3760 #x3f81111de524b6f0)))) ; sine approximation used when calculating cos.  ~ x - 1/6x³ + 1/120x⁵
  (unless (boundp '+cos-cosine-polynomial+)
    (defconstant +cos-cosine-polynomial+ (mapcar #'u2d '(#x3feffffffffffffc #xbfdffffffff83643 #x3fa555488594da9d)))) ; cosine approximation used when calculating cos.  ~ 1 - 1/2x² + 1/24x⁴
  )

;; range-reduce-large: given xi the integer representation of a (positive) floating-point number greater than or equal to +range-reduce-large-threshold+, return (values q r) the quotient and remainder following division by pi/256.  Latter is a fractional float, former is a 9-bit integer
(flet ((range-reduce-large (xi)
         ;; todo can tweak e and m and implement this as an unaligned memory op
         (let* ((e (+ -127 (ldb (byte 8 23) xi)))
                ;; extract mantissa.  Will always be normal, since we've already taken care of the denormal range, so this is cheaper than integer-decode-float
                (m (logior (ash 1 23) (ldb (byte 23 0) xi)))
                ;; 256-bit fixedpoint pi.  Might be a bit faster to do the math by hand, as the c version does, but meh
                (qb (* m #xa2f9836e4e441529fc2757d1f534ddc0db6295993c439041fe5163abdebbc562))
                (i (- 208 e))
                ;; only need 9 of these bits; 8 are fractional (pi/256), 9th is sign
                (q (ldb (byte 9 (+ 64 i)) qb))
                ;; need all 64 bits, unfortunately
                (r (ldb (byte 64 i) qb)))
           (values q (* (coerce r 'double-float) #.(scale-float 1d0 -64))))) ; r 0.64 fixedpoint->fractional
       ;; same result spec.  x is a single float
       (range-reduce-small-cos (x &aux (xd (coerce x 'double-float)))
         (let* ((q (the (double-float 1d0 1.8d8) (* xd (u2d #x40545f306c000000)))) ; first quotient approximation; multiply by ~256/pi
                (tq (ftrunc q))                  ; truncated quotient
                (iq (the (integer 0 170891318) (truncate q))) ; integer quotient
                (r (- q tq))                        ; remainder estimate
                (r (fma-double r xd (u2d #x3e9c9c882a53f84f)))) ; refine remainder estimate using some lower bits of 256/pi
           (if (> r 1d0)                            ; account if we overestimated (todo should be >=?)
               (values (1+ iq) (1- r))
               (values iq r)))))
  (defun cos-single (x &aux (xa (abs x)) (xi (s2u xa)))
    (cond
      ((< xa (u2s +cos-one-threshold+)) 1s0)
      ;;((>= xi #x7f800000) (- x x)) ; infinities and nans live there.  NB. this can be handled w/o a branch using avx512 fixup.  Or we can ensure the following computations induce nan/trap for inf/nan input?
      ((< xa (u2s +cos-simple-threshold+))
       (coerce
        (even-polynomial double-float
                         (coerce x 'double-float)
                         . #.+cos-simple-polynomial+)
        'single-float))
      (t
       ;; slower path; start with range reduction
       ;; reduce x modulo 256*pi
       (multiple-value-bind (q r) ; quotient and fractional remainder after division by pi
           (if (< xa (u2s +range-reduce-large-threshold+))
               ;; <2097152; numbers in this range can be range-reduced with a lower-precision pi constant
               (range-reduce-small-cos xa)
               ;; >=2097152; numbers in this range need a higher-precision pi constant
               ;; early-out on what I assume are hard-to-round cases
               #.`(cond ,@(loop for (k . v) in +cos-hard-to-round-cases+
                                collect `((eql x ,(u2s k)) (return-from cos-single ,(u2s v))))
                        (t (range-reduce-large xi))))
         ;; defractionalise remainder
         (let ((r (* r +pi/256+))
               (q (+ 128 q))) ;why do we add 128 here?
           ;; now we have x = r+πq/256.  Compute cos(x) = cos(r+πq/256) = cos(r-(-πq/256)) = sin(r)sin(πq/256)+cos(r)cos(πq/256).  r is handled by polynomial approximation, and q by table lookup
           ;; grab low 7 bits, i.e. x divided by and mod (quantised to π/256) π/2
           (multiple-value-bind (qq qr) (floor q 128)
             ;; we want to use qr as an index into the lookup table.  If the low bit of qq is set then we are on the 'falling' portion of the cosine wave, and actually want to index with 128-qr, (and commensurately replace r with π/256-r); do this with bithacks
             (let* ((r (if (logbitp 0 qq) (- +pi/256+ r) r))
                    ;; (logand 128 q) will be 128 iff (lognot (1- (logand 1 qq))) is -1, but I don't think sbcl will figure this out
                    (qi (the (integer 0 127)
                             (+ (logand 128 q)
                                (logxor qr (lognot (1- (logand 1 qq))))))))
               ;; don't need to account for the sign of cosq/sinq; we will correct it in the result
               (let ((sinq (aref +double-sin-cos-*256+ qi 1))
                     (cosq (aref +double-sin-cos-*256+ qi 0)))
                 (let ((sinr (odd-polynomial double-float r . #.+cos-sine-polynomial+))
                       (cosr (even-polynomial double-float r . #.+cos-cosine-polynomial+)))
                   (+ (- x x)
                      (* (if (logbitp 1 qq) -1s0 1s0)
                         (coerce (fma-double (* cosq cosr) sinq sinr)
                                 'single-float)))))))))))))
             
