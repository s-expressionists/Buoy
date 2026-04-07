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
