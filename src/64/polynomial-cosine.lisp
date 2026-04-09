(cl:in-package #:buoy-core-math-64)

(defparameter *fast-polynomial-cosine-table*
  (make-array
   5
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (p "0x1.0p+0")
           (p "-0x1.923015cp-77")         ; degree 0
           (p "-0x1.3bd3cc9be45dep4")     ; degree 2
           (p "0x1.03c1f080ad892p6")      ; degree 4
           (p "-0x1.55a5c590f9e6ap6"))))) ; degree 6


;;; Return two values representing an approximation of (cos (* 2 pi (+
;;; x-high x-low))) for the sum of x-high and x-low greater than or
;;; equal to (expt 2 -24) and less than (+ (expt 2 -11) (expt 2 -24))
;;; with a relative error less than (expt 2 -69.96.  Also, (< (abs
;;; x-low) (expt 2 -52.36)) holds.  Assume the sum of u-high and u-low
;;; approximates the square of the sum of x-high and x-low.  However,
;;; since consine is an even function, only even degrees of the
;;; arguments are used.  For that reason, x-high and x-low are not
;;; needed, so not passed as arguments; only u-high and u-low are
;;; needed.
(defun eval-fast-polynomial-cosine (u-high u-low)
  (let* ((table *fast-polynomial-cosine-table*)
         (high (aref table 4))
         (high (fma high u-high (aref table 3)))
         (high (fma high u-high (aref table 2))))
    (multiple-value-bind (high low)
        (s-multiply high u-high u-low)
      (multiple-value-bind (high tt)
          (fast-two-sum (aref table 0) high)
        (incf low (+ (aref table 1) tt))
        (values high low)))))


;;; The following is a degree-10 polynomial with even coefficients
;;; approximating (cos (* 2 pi x)) for x greater than or equal to 0
;;; and less than (expt 2 -11) with relative error (expt 2 -137.246).

;; static const dint64_t PC[] = {
(defparameter *polynomial-cosine-table*
  (make-array
   6
   :initial-contents
   (flet ((m (&rest arguments)
            (apply #'make-custom-float-64 arguments)))
     (list 
      (m :high #x8000000000000000
         :low #x0
         :exponent 1 :sign 0)           ; degree 0
      (m :high #x9de9e64df22ef2d2
         :low #x56e26cd9808c1949
         :exponent 5 :sign 1)           ; degree 2
      (m :high #x81e0f840dad61d9a
         :low #x9980f00630cb655e
         :exponent 7 :sign 0)           ; degree 4
      (m :high #xaae9e3f1e5ffcfe2
         :low #xa508509534006249
         :exponent 7 :sign 1)           ; degree 6   
      (m :high #xf0fa83448dd1e094
         :low #xe0603ce7044eeba
         :exponent 6 :sign 0)           ; degree 8
      (m :high #xd368f6f4207cfe49
         :low #xec63157807ebffa
         :exponent 5 :sign 1)))))       ; degree 10

;;; Compute an approximation of (COS (* 2 PI X) for x greater than or
;;; equal to 0 and less than (EXPT 2 11).  Put the result in the
;;; custom float 64 passed as the first argument.  Since cosine is an
;;; even function, we do not actually need X, so we pass only
;;; X-SQUARED as an argument.
(defun eval-polynomial-cosine (result x-squared)
  (let ((table  *polynomial-cosine-table*))
    (multiply-custom-float-64 result x-squared (aref table 5)) ; degree 10
    (add-custom-float-64 result result (aref table 4))         ; degree 8
    (multiply-custom-float-64 result result x-squared)
    (add-custom-float-64 result result (aref table 3))         ; degree 6
    (multiply-custom-float-64 result result x-squared)
    (add-custom-float-64 result result (aref table 2))         ; degree 4
    (multiply-custom-float-64 result result x-squared)
    (add-custom-float-64 result result (aref table 1))         ; degree 2
    (multiply-custom-float-64 result result x-squared)
    (add-custom-float-64 result result (aref table 0))))       ; degree 0
