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

;;; The comment for this function in the core-math library does not
;;; correspond to the code.  So I don't know whether this definition
;;; is correct.
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
