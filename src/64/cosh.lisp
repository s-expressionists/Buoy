(cl:in-package #:buoy-core-math-64)

;;; The function cosh(x) is approximated by a minimax polynomial
;;; cosh(x)~1+x^2*P(x^2) for |x|<0.125. For other arguments the
;;; identity cosh(x)=(exp(|x|)+exp(-|x|))/2 is used. For |x|<5 both
;;; exponents are calculated with slightly higher precision than
;;; double. For 5<|x|<36.736801, exp(-|x|) is rather small and is
;;; calculated with double precision but exp(|x|) is calculated with
;;; higher than double precision. For 36.736801<|x|<710.47586
;;; exp(-|x|) becomes too small and only exp(|x|) is calculated.
