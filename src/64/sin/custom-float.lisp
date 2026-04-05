(cl:in-package #:buoy)

;;; This struct definition is used as a custom float type.  The value
;;; represented is (* (/ high 64) (/ low 128) (expt 2 (1+ exponent))).
;;; A sign slot of 0 means the value represented is positive and a
;;; sign slot of 1 mean the value represented is negative.

(defstruct custom-float-64
  (high 0 :type (unsigned-byte 64))
  (low 0 :type (unsigned-byte 64))
  (exponent 0 :type (signed-byte 64))
  (sign 0 :type (unsigned-byte 64)))

(defparameter *1*
  (make-custom-float-64
   :high #x8000000000000000
   :low #x0
   :exponent 0
   :sign 0))

(defparameter *-1*
  (make-custom-float-64
   :high #x8000000000000000
   :low #x0
   :exponent 0
   :sign 1))

(defparameter *e-rational*
  (let ((e-string "2718281828459045235360287471352662497757247093699959574966"))
    (/ (read-from-string e-string)
       (expt 10 (1- (length e-string))))))

(defparameter *log2*
  (let* ((stuff (- 1 (/ 2 *e-rational*)))
         (log2-rational
           (- 1 (loop for i from 1 to 128
                      for numerator = stuff then (* numerator stuff)
                      sum (/ numerator i))))
         (bits (round (* (expt 2 128) log2-rational))))
    (make-custom-float-64
     :high (ldb (byte 64 64) bits)
     :low (ldb (byte 64 0) bits)
     :exponent -1
     :sign 0)))
     
