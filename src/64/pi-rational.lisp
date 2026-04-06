(cl:in-package #:buoy-core-math-64)

;;; For SIN or COS, we may get arguments that are as large as (EXPT 2
;;; 1024).  We need to compute such a large argument modulo (* 2 pi).
;;; This operation is basically equivalent to subtracting (* 2 pi)
;;; from the argument N times until what remains is less than (* 2
;;; pi).  Each such subtraction introduces an error corresponding to
;;; the precision with which we compute (* 2 pi).  For that reason, we
;;; need a very high precision, i.e., more than (* 0.3010 1024) which
;;; is more than 308.

(defparameter +pi-rational+
  (let ((pi-string "3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609"))
    (/ (read-from-string pi-string)
       (expt 10 (1- (length pi-string))))))

;;; We now compute a table of 20 64-bit unsigned values that together
;;; represent an approximation of (/ (* 2 pi)) multiplied by (EXPT 2
;;; 1280)

(defparameter *pi-table*
  (let* ((1/2pi (/ (* 2 +pi-rational+)))
         (stuff (round (* 1/2pi (ash 1 1280))))
         (result (make-array 20 :element-type '(unsigned-byte 64))))
    (loop for index from 19 downto 0
          for position from 0 by 64
          do (setf (aref result index)
                   (ldb (byte 64 position) stuff)))
    result))
