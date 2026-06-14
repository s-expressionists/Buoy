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
         (v0 (fma ax s (parse-c-literal "0x1.8000002p+26")))
         (jtu (f-to-i v0))
         (v jtu)
         (tt 
    
