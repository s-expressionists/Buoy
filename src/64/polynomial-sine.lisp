(cl:in-package #:buoy-core-math-64)

;;; The following is a degree-7 polynomial with odd coefficients
;;; approximating sin2pi(x) for -2^-24 < x < 2^-11+2^-24 with relative
;;; error 2^-77.306. Generated with sin_fast.sollya.
;;;
;;; Here is how I think this works.  The polynomial is roughly 6x -
;;; 40x^3 + 80x^5 - 75x^7.  Now if we want a relative error of
;;; 2^-77.306, we must evaluate this polynomial with a "double
;;; double", so that the high part is less than roughly 2^-11, and
;;; therefore the low part is something like 2^-63 (given the
;;; precision of a double float).  Now, let's examine the term 40x^3.
;;; First of all, x^-11 is less than around 2^-33 and even if x is
;;; represented as only the high part of the double double, we can
;;; express x as 2^-11 +- 2^-64 so the error is roughly 2*2^-22*2-64
;;; which is 2^-85.  Multiplied by 40, (say 2^6) we have 2^-79.
;;; Compared to the first term, this is small enough that it can be
;;; ignored.  A similar argument can be made for the precision of the
;;; coefficient of the X^3 term.  This is why that coefficient can be
;;; represented as only one double float.  A similar (but even easier)
;;; argument can be made for all other terms except the first one.  So
;;; we can use only the high part of x for all terms except the first
;;; one, and we must use both a coefficient and a value of x with more
;;; precision for the first term. --RS 2026-05-20
;;;
;;; In fact, those coefficients should be close to the coefficients of
;;; the Taylor expansion, and this is indeed the case. 
(defparameter  *fast-polynomial-sine-table*
  (make-array
   5
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (p "0x1.921fb54442d18p+2")
           (p "0x1.1a62645446203p-52")     ; degree 1 (h+l)
           (p "-0x1.4abbce625be53p5")      ; degree 3
           (p "0x1.466bc678d8d63p6")       ; degree 5
           (p "-0x1.331554ca19669p6")))))  ; degree 7

;;; Return two values representing an approximation of (sin (* 2 pi (+
;;; x-high x-low))) for the sum of x-high and x-low greater than or
;;; equal to (expt 2 -24) and less than (+ (expt 2 -11) (expt 2 -24)).
;;; Assume the sum of u-high and u-low approximates the square of the
;;; sum of x-high and x-low.
(defun eval-fast-polynomial-sine (x-high x-low u-high u-low)
  (let* ((table *fast-polynomial-sine-table*)
         (high (aref table 4))
         (high (fma high u-high (aref table 3)))
         (high (fma high u-high (aref table 2))))
    (multiple-value-bind (high low)
        (s-multiply high u-high u-low)
      (multiple-value-bind (high tt)
          (fast-two-sum (aref table 0) high)
        (incf low (+ (aref table 1) tt))
        (d-multiply high low x-high x-low)))))

(defparameter *polynomial-sine-table*
  (make-array
   6
   :initial-contents
   (list 
    (make-custom-float-64
     :high #xc90fdaa22168c234
     :low #xc4c6628b80dc1cd1
     :exponent 3 :sign 0) ; 1
    (make-custom-float-64
     :high #xa55de7312df295f5
     :low #x5dc72f712aa57db4
     :exponent 6 :sign 1) ; 3
    (make-custom-float-64
     :high #xa335e33bad570e92
     :low #x3f33be0021aa54d2
     :exponent 7 :sign 0) ; 5
    (make-custom-float-64
     :high #x9969667315ec2d9d
     :low #xe59d6ab8509a2025
     :exponent 7 :sign 1) ; 7
    (make-custom-float-64
     :high #xa83c1a43bf1c6485
     :low #x7d5f8f76fa7d74ed
     :exponent 6 :sign 0) ; 9
    (make-custom-float-64
     :high #xf16ab2898eae62f9
     :low #xa7f0339113b8b3c5
     :exponent 4 :sign 1)))) ; 11

(defun eval-polynomial-sine (destination x x-squared)
  (let ((table *polynomial-sine-table*))
    (multiply-custom-float-64 destination x-squared (aref table 5))
    (add-custom-float-64 destination destination (aref table 4))
    (multiply-custom-float-64 destination destination x-squared)
    (add-custom-float-64 destination destination (aref table 3))
    (multiply-custom-float-64 destination destination x-squared)
    (add-custom-float-64 destination destination (aref table 2))
    (multiply-custom-float-64 destination destination x-squared)
    (add-custom-float-64 destination destination (aref table 1))
    (multiply-custom-float-64 destination destination x-squared)
    (add-custom-float-64 destination destination (aref table 0))
    (multiply-custom-float-64 destination destination x)))
