(cl:in-package #:buoy-core-math-64)

;;; This struct definition is used as a custom float type.  The value
;;; represented is (* (/ high (expt 2 64)) (/ low (expt 2 128)) (expt
;;; 2 (1+ exponent))).  A sign slot of 0 means the value represented
;;; is positive and a sign slot of 1 mean the value represented is
;;; negative.

(defstruct (custom-float-64 (:conc-name nil))
  (high 0 :type (unsigned-byte 64))
  (low 0 :type (unsigned-byte 64))
  (exponent 0 :type (signed-byte 64))
  (sign 0 :type (unsigned-byte 64)))

(defun rational-from-custum-float-64 (custom-float-64)
  (* (if (zerop (sign custom-float-64)) 1 -1)
     (+ (/ (high custom-float-64) (ash 1 64))
        (/ (low custom-float-64) (ash 1 128)))
     (expt 2 (1+ (exponent custom-float-64)))))

(defun custom-float-64-from-rational (rational)
  (let* ((sign (if (minusp rational) 1 0))
         (numerator (abs (numerator rational)))
         (numerator-length (integer-length numerator))
         (denominator (denominator rational))
         (denominator-length (integer-length denominator))
         (diff (- numerator-length denominator-length))
         bits)
    (if (minusp diff)
        ;; Then we have a small number
        (setf numerator (ash numerator (- diff)))
        ;; Otherwise we have a large number.
        (setf denominator (ash denominator diff)))
    ;; There are now two possibilities: either the numerator is
    ;; smaller than the denominator, so that the quotient is greater
    ;; than 1/2 but less than 1.  Otherwise, the quotient is greater
    ;; than 1 but less than 2.
    (if (< numerator denominator)
        (progn (setf bits (round (/ (ash numerator 128) denominator)))
               (decf diff))
        (setf bits (round (/ (ash numerator 127) denominator))))
    (make-custom-float-64
     :high (ldb (byte 64 64) bits)
     :low (ldb (byte 64 0) bits)
     :exponent diff
     :sign sign)))

(defparameter *1* (custom-float-64-from-rational 1))

(defparameter *-1* (custom-float-64-from-rational -1))

(defparameter *e-rational*
  (let ((e-string "2718281828459045235360287471352662497757247093699959574966"))
    (/ (read-from-string e-string)
       (expt 10 (1- (length e-string))))))

;;; The Taylor series for (LOG 2) has very slow convergence, so we
;;; rewrite it as (+ 1 (LOG (/ 2 *e-rational*))) which converges much
;;; faster.

(defparameter *log2-rational*
  (let ((stuff (- 1 (/ 2 *e-rational*))))
    (- 1 (loop for i from 1 to 128
               for numerator = stuff then (* numerator stuff)
               sum (/ numerator i)))))

(defparameter *log2* (custom-float-64-from-rational *log2-rational*))
