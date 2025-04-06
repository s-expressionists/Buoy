(cl:in-package #:buoy)

;;; I haven't actually coputed the required precision for
;;; +pi-rational+, nor the number of required iterations in the
;;; Maclaurin series in order to get an accurate rational
;;; approximation of the sine of an angle.  It could be significantly
;;; less than I have here, which would make the table computation
;;; faster.

(defparameter +pi-rational+
  (let ((pi-string "31415926535897932384626433832795028841971"))
    (/ (read-from-string pi-string)
       (expt 10 (1- (length pi-string))))))

(defun sin-rational (x)
  (loop for i from 1 to 200 by 2
        for sign = 1 then (- sign)
        for factorial = 1 then (* factorial (1- i) i)
        sum (* sign (/ (expt x i) factorial))))

;;; The core-math library uses a single table with C structs in it,
;;; but we want to store 64-bit values without creating bignums, so we
;;; use separate tables where the element type is (UNSIGNED-BYTE 64),
;;; hoping that the implementation has a specialized vector type for
;;; that element type.

(defvar *sin-table-high*)

(defvar *sin-table-low*)

(defvar *sin-table-exponent*)

(defun compute-sin-tables ()
  (setf *sin-table-high* (make-array 256 :element-type '(unsigned-byte 64)))
  (setf *sin-table-low* (make-array 256 :element-type '(unsigned-byte 64)))
  (setf *sin-table-exponent* (make-array 256 :element-type '(integer -8 0)))
  (setf (aref *sin-table-high* 0) 0)
  (setf (aref *sin-table-low* 0) 0)
  (setf (aref *sin-table-exponent* 128) 0)
  (loop for i from 1 below 256
        for sin-rational = (sin-rational (* +pi-rational+ (/ i 1024)))
        for exponent = 0
        do (loop while (< sin-rational 1/2)
                 do (decf exponent)
                    (setf sin-rational (* sin-rational 2)))
           (let ((128-bit-value (round (* sin-rational (expt 2 128)))))
             (setf (aref *sin-table-high* i)
                   (ldb (byte 64 64) 128-bit-value))
             (setf (aref *sin-table-low* i)
                   (ldb (byte 64 0) 128-bit-value))
             (setf (aref *sin-table-exponent* i) exponent))))

(defun cos-rational (x)
  (1+ (loop for i from 2 to 200 by 2
            for sign = -1 then (- sign)
            for factorial = 2 then (* factorial (1- i) i)
            sum (* sign (/ (expt x i) factorial)))))
