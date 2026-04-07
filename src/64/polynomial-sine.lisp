(cl:in-package #:buoy-core-math-64)

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
