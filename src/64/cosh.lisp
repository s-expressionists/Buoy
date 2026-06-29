(cl:in-package #:buoy-core-math-64)

;;; The function cosh(x) is approximated by a minimax polynomial
;;; cosh(x)~1+x^2*P(x^2) for |x|<0.125. For other arguments the
;;; identity cosh(x)=(exp(|x|)+exp(-|x|))/2 is used. For |x|<5 both
;;; exponents are calculated with slightly higher precision than
;;; double. For 5<|x|<36.736801, exp(-|x|) is rather small and is
;;; calculated with double precision but exp(|x|) is calculated with
;;; higher than double precision. For 36.736801<|x|<710.47586
;;; exp(-|x|) becomes too small and only exp(|x|) is calculated,
;;; yielding an accuracy of 106 bits.

(defparameter *cosh-ch-table*
  (make-array
   '(4 2)
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (list (p "0x1.0p-1")
                 (p "-0x1.c7e8db669f624p-111"))   ; degree 2
           (list (p "{0x1.5555555555555p-5")
                 (p "0x1.5555555556135p-59"))     ; degree 4
           (list (p "{0x1.6c16c16c16c17p-10")
                 (p "-0x1.f49f4a6e838f2p-65"))    ; degree 6
           (list (p "0x1.a01a01a01a01ap-16")
                 (p "0x1.a4ffbe15316aap-76")))))) ; degree 8

(defparameter *cosh-cl-table*
  (make-array
   4
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (p "0x1.27e4fb7789f5cp-22")     ; degree 10
           (p "0x1.1eed8eff9089cp-29")     ; degree 12
           (p "0x1.939749ce13dadp-37")     ; degree 14
           (p "0x1.ae9891efb6691p-45"))))) ; degree 16
   
(defun as-cosh-zero (x)
  (let ((ch *cosh-ch-table*)
        (cl *cosh-cl-table*))
    (flet ((cl (i) (aref cl i)))
      (let* ((x2 (* x x))
             (x2l (fma x x (- x2)))
             (y2 (* x2 (+ (cl 0)
                          (* x2 (+ (cl 1)
                                   (* x2 (+ (cl 2)
                                            (* x2 (cl 3)))))))))
             (y1 0d0)
             (y0 0d0))
        (multiple-value-setq (y1 y2)
          (cosh-poly-dd x2 x2l 4 ch))
        (multiple-value-setq (y1 y2)
          (multiply-dd y1 y2 x2 x2l))
        (multiple-value-setq (y0 y1)
          (fast-two-sum y1 y2))
        (let ((tu (f-to-i 'double-float y1)))
          (when (zerop (logand tu (1- (ash 1 52))))
            (let ((wu (f-to-i 'double-float y2)))
              (if (zerop (ash (logxor wu tu) -63))
                  (incf tu)
                  (decf tu))))
          (if (= (logand tu (1- (ash 1 52))) (1- (ash 1 52)))
              (as-cosh-database x (+ y0 y1))
              (+ y0 y1)))))))

(defparameter *cosh-database*
  (make-array
   '(21 3)
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (list (p "0x1.9a5e3cbe1985ep-4")
                 (p "0x1.01492f72f984bp+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.52a11832e847dp-3")
                 (p "0x1.0381e68cac923p+0")
                 (p "0x1.0p-104"))
           (list (p "0x1.bf0305e2c6c37p-3")
                 (p "0x1.061f4c39e16f2p+0")
                 (p "0x1.0p-107"))
           (list (p "0x1.17326ffc09f68p-2")
                 (p "0x1.099318a43ac8p+0")
                 (p "0x1.0p-104"))
           (list (p "0x1.3d27bf16d8bdbp-2")
                 (p "0x1.0c6091056e06ap+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.03923f2b47c07p-1")
                 (p "0x1.219c1989e3373p+0")
                 (p "-0x1.0p-54"))
           (list (p "0x1.a6031cd5f93bap-1")
                 (p "0x1.5bff041b260fep+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.104b648f113a1p+0")
                 (p "0x1.9efdca62b700ap+0")
                 (p "-0x1.0p-109"))
           (list (p "0x1.1585720f35cd9p+0")
                 (p "0x1.a5bf3acfde4b2p+0")
                 (p "0x1.0p-105"))
           (list (p "0x1.e9cc7ed2e1a7ep+0")
                 (p "0x1.bb0ff220d8eb5p+1")
                 (p "-0x1.0p-53"))
           (list (p "0x1.43180ea854696p+1")
                 (p "0x1.91f1122b6b63ap+2")
                 (p "0x1.0p-102"))
           (list (p "0x1.725811dcf6782p+2")
                 (p "0x1.45ea160ddc71fp+7")
                 (p "-0x1.0p-100"))
           (list (p "0x1.5afd56f7d565bp+3")
                 (p "0x1.8ff8e0ccea7cp+14")
                 (p "0x1.0p-90"))
           (list (p "0x1.759a2ad4c4d56p+3")
                 (p "0x1.cb62eec26bd78p+15")
                 (p "-0x1.0p-92"))
           (list (p "0x1.7fce95ea5c653p+3")
                 (p "0x1.3bf8009648dcp+16")
                 (p "0x1.0p-88"))
           (list (p "0x1.743d5609348acp+4")
                 (p "0x1.7a87a8bb7fa28p+32")
                 (p "-0x1.0p-22"))
           (list (p "0x1.e07e71bfcf06fp+5")
                 (p "0x1.91ec4412c344fp+85")
                 (p "0x1.0p-24"))
           (list (p "0x1.6474c604cc0d7p+6")
                 (p "0x1.7a8f65ad009bdp+127")
                 (p "-0x1.0p+20"))
           (list (p "0x1.54cd1fea7663ap+7")
                 (p "0x1.c90810d354618p+244")
                 (p "0x1.0p+135"))
           (list (p "0x1.2da9e5e6af0bp+8")
                 (p "0x1.27d6fe867d6f6p+434")
                 (p "0x1.0p+329"))
           (list (p "0x1.d6479eba7c971p+8")
                 (p "0x1.62a88613629b6p+677")
                 (p "-0x1.0p+568"))))))

;;; This function does a binary search, but it tests for equality in
;;; each iteration, which doubles the number of tests.  But then, this
;;; function is probably not executed very often.  It also has two
;;; occurrences of (floor (+ a b) 2) which is unnecessary.  There
;;; should be a single occurrence at the beginning of the loop.  Also,
;;; there is no point in assigning to F and then do a local control
;;; transfer.  Might as well return from the function right there.
(defun as-cosh-database (x f)
  (let* ((tt *cosh-database*)
         (a 0)
         (b (1- (array-dimension tt 0)))
         (m (floor (+ a b) 2))
         (ax (abs x)))
    (loop while (<= a b)
          do (cond ((< (aref tt m 0) ax)
                    (setf a (1+ m)))
                   ((= (aref tt m 0) ax)
                    (setf f (+ (aref tt m 1) (aref tt m 2)))
                    (loop-finish))
                   (t
                    (setf b (1- m))))
             (setf m (floor (+ a b) 2)))
    f))

(defun cosh-2^-26<=x<1/4 (x)
  ;; q(x) = 1 + c0*x^2 + c1*x^4 + c2*x^6 + c3*x^8 + c4*x^10 is a
  ;; degree-10 polynomial approximating cosh(x) on [2^-26, 0.125] such
  ;; that: |q(x) - cosh(x)| < 2^-67.518 * x^2.  This polynomial was
  ;; generated with the following Sollya command: d = [2^-26,0.125];
  ;; q=1+x^2*fpminimax((cosh(x)-1)/x^2, [|0,2,4,6,8|], [|53...|], d,
  ;; absolute);
  (let ((c0 #.(parse-c-literal "0x1.0p-1"))
        (c1 #.(parse-c-literal "0x1.5555555555554p-5"))
        (c2 #.(parse-c-literal "0x1.6c16c16c1d0cp-10"))
        (c3 #.(parse-c-literal "0x1.a01a0075066b4p-16"))
        (c4 #.(parse-c-literal "0x1.27faff8dcc1c8p-22")))
    (let* ((x2 (* x x))
           (x4 (* x2 x2))
           (p (* x2 (+ (+ c0 (* x2 c1))
                       (* x4 (+ (+ c2 (* x2 c3))
                                (* x4 c4))))))
           ;; fails with e = x2*(0x1.c8p-52), x=0x1.0f0a7d6ea89ep-14
           ;; (rndu, no FMA)
           (e (* x2 #.(parse-c-literal "0x1.84p-51")))
           (lb (+ 1 (- p e)))
           (ub (+ 1 (+ p e))))
      (if (= lb ub)
          lb
          (as-cosh-zero x)))))

(defun cosh-0<=x<1/4 (x)
  (if (< x #.(sim:dfloat (expt 2 -26)))
      1d0
      (cosh-2^-26<=x<1/4)))

(defun compute-h-l (x0 x1)
  (let ((t0 *t0-table*)
        (t1 *t1-table*))
    (let* ((0h (aref t0 x0 1))
           (0l (aref t0 x0 0))
           (1h (aref t1 x1 1))
           (1l (aref t1 x1 0))
           (h (* 0h 1h))
           (l (+ (* 0h 1l) (* 1h 0l) (fma 0h 1h (- h)))))
      (values h l))))

(defun cosh-1/4<=x<=5 (x)
  (let* ((s +2^12/LN-2+)
         ;; By adding 0x1.8000002p+26, the rounded integer part of x*s
         ;; ends up in bits 47-26 (22 bits) of the result.  So we are
         ;; going to compute 2^(i+f) where i the integer part and f is
         ;; the fractional part.  That is 2^i*2^f where 2^i is the
         ;; exponent of the result.
         (v0 (fma x s #.(parse-c-literal "0x1.8000002p+26")))
         (jtu (f-to-i 'double-float v0))
         (vu (f-to-i 'double-float v0))
         ;; tt is an integer with 39 1s followed by 25 0s.
         (tt #.(ash (1- (ash 1 39)) 25))
         ;; VU has the fractional bits of V0 eliminated, except for
         ;; bit 26 which is still what it was.
         (vu (logand vu tt))
         ;;; TTT now contains the integer part of x*s, except that 0.5
         ;;; has been added to it if and only if the integer part was
         ;;; not the result of rounding up.
         (ttt (- (i-to-f 'double-float vu)
                 #.(parse-c-literal "0x1.8p26")))
         (aix (f-to-i 'double-float x))
         ;; IL contains the integer part of x*s.
         (il (ldb (byte 10 26) jtu))
         (jl (- il))
         ;; I1 contains the lower 6 bits of the integer part of x*s.
         (i1 (logand il #x3f))
         ;; I0 contains bits 11-6 of the integer part of x*s.
         (i0 (logand (ash il -6) #x3f))
         ;; IE contains the upper 10 bits of the integer part of x*s
         (ie (ash il -12))
         (j1 (logand jl #x3f))
         (j0 (logand (ash jl -6) #x3f))
         (je (ash jl -12))
         (l2h #.(parse-c-literal "0x1.62e42ffp-13"))
         (l2l #.(parse-c-literal "0x1.718432a1b0e26p-47"))
         (ch0 #.(parse-c-literal "0x1.0p+0"))
         (ch1 #.(parse-c-literal "0x1.0p-1"))
         (ch2 #.(parse-c-literal "0x1.5555555aaaaaep-3"))
         (ch3 #.(parse-c-literal "0x1.55555551c98cp-5"))
    (multiple-value-bind (th tl)
        (compute-h-l i0 i1)
      (let* ((dx (+ (- x (*  l2h ttt)) (* l2l ttt)))
             (dx2 (* dx dx))
             (pp (* dx (+ (+ ch0 (* dx ch1))
                          (* dx2 (+ ch2 (* dx ch3))))))
             (rh 0d0)
             (rl 0d0))
        
      
         
    

  (multiple-value-bind (qh ql)
      (compute-h-l j0 j1)
    

(defun cosh-5<x<=max (x)
  (if (> x 36.736801d0)
      (cosh-36.736801d0<x<=max x)
      (cosh-5<x<=36.736801d0 x)))

(defun cosh-1/4<=x<=max (x)
  (if (> x 5d0)
      (cosh-5<x<=max x)
      (cosh-1/4<=x<=5 x)))

(defun cosh-x>=1/4 (x)
  (if (> x #.(parse-c-literal "0x1.633ce8fb9f87dp+9"))
      (error 'floating-point-overflow)
      (cosh-1/4<=x<=max x)))

(defun cosh-x>=0 (x)
  (if (< x #.(sim:dfloat 1/4))
      (cosh-0<=x<1/4)
      (cosh-x>=1/4)))

(defun cr-cosh (x)
  (cosh-x>=0 (abs x)))
