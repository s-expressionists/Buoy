(cl:in-package #:buoy-core-math-64)

(defparameter *t-inv-table*
  (make-array
   256
   :element-type '(unsigned-byte 64)
   :initial-contents
   (loop for i from 0 below 256
         collect (floor (/ (expt 2 127)
                           (+ (expt 2 63)
                              (* i (expt 2 55))
                              (expt 2 55)
                              -1))))))

;;; For |x| <= 0x1.d12ed0af1a27ep-27, tan(x) rounds to x (to nearest):
;;; we can assume x >= 0 without loss of generality since tan(-x) =
;;; -tan(x), we have x < tan(x) < x + x^3/3 for say 0 < x <= 1 thus
;;; |tan(x) - x| < x^3/3. Write x = c*2^e with 1/2 <= c < 1. Then
;;; ulp(x)/2 = 2^(e-54), and x^3/3 = c^3/3*2^(3e), thus x^3/3 <
;;; ulp(x)/2 rewrites as c^3/3*2^(3e) < 2^(e-54), or c^3*2^(2e+54) < 3
;;; (1). For e <= -27, since c^3 < 1, we have c^3*2^(2e+54) < 1 <
;;; 3. For e=-26, (1) rewrites 4*c^3 < 3 which yields c <=
;;; 0x1.d12ed0af1a27ep-1.

(defun cr-tan (x)
  (let ((abs-x (abs x)))
    (cond ((infinity-or-nan-p x)
           (error 'type-error
                  :datum x
                  :expected-type 'double-float))
          ((< abs-x #.(parse-c-literal "0x1.d12ed0af1a27ep-27"))
           (fma x #.(parse-c-literal "0x1.0p-54") x))
          (t
           (cr-tan-normal-number x)))))
