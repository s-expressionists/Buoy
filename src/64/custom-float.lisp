(cl:in-package #:buoy-core-math-64)

;;; This struct definition is used as a custom float type.  The value
;;; represented is (* (/ high (expt 2 64)) (/ low (expt 2 128)) (expt
;;; 2 exponent)).  A sign slot of 0 means the value represented
;;; is positive and a sign slot of 1 mean the value represented is
;;; negative.

(defstruct (custom-float-64 (:conc-name nil) (:copier nil))
  (high 0 :type (unsigned-byte 64))
  (low 0 :type (unsigned-byte 64))
  (exponent 0 :type (signed-byte 64))
  (sign 0 :type (unsigned-byte 64)))

(defun rational-from-custom-float-64 (custom-float-64)
  (* (if (zerop (sign custom-float-64)) 1 -1)
     (+ (/ (high custom-float-64) (ash 1 64))
        (/ (low custom-float-64) (ash 1 128)))
     (expt 2 (exponent custom-float-64))))

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
        (setf bits (round (/ (ash numerator 128) denominator)))
        (progn 
          (setf bits (round (/ (ash numerator 127) denominator)))
          (incf diff)))
    (make-custom-float-64
     :high (ldb (byte 64 64) bits)
     :low (ldb (byte 64 0) bits)
     :exponent diff
     :sign sign)))

;;; FIXME: do this better
(defun custom-float-64-from-double-float (double-float)
  (custom-float-64-from-rational (rational double-float)))

;;; FIXME: do this better
(defun double-float-from-custom-float-64 (custom-float-64)
  (float (rational-from-custom-float-64 custom-float-64) 1d0))

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

;;; We define this function as we do in order to avoid allocating
;;; memory.  The hope is that the destination will be supplied by the
;;; caller, and that it can be considered as having dynamic extent.
(defun copy-custom-float-64 (destination source)
  (declare (type custom-float-64 destination source))
  (setf (low destination) (low source)
        (high destination) (high source)
        (exponent destination) (exponent source)
        (sign destination) (sign source)))

(defun custom-float-64-zerop (custom-float-64)
  (zerop (high custom-float-64)))

;;; We define operations on custom floats so that the result is
;;; obtained by filling the slots of a CUSTOM-FLOAT-64 passes as the
;;; first argument, again to avoid allocating memory.

;;; I am lazy here so I allocate memory and do a lot of unnecessary
;;; computations, but this must be improved to obtain good
;;; performance.
(defun add-custom-float-64 (destination x y)
  (let ((sum (custom-float-64-from-rational
              (+ (rational-from-custom-float-64 x)
                 (rational-from-custom-float-64 y)))))
    (copy-custom-float-64 destination sum)))

(defun multiply-custom-float-64 (destination x y)
  (let ((product (custom-float-64-from-rational
                  (* (rational-from-custom-float-64 x)
                     (rational-from-custom-float-64 y)))))
    (copy-custom-float-64 destination product)))

(defun normalize-custom-float-64 (custom-float-64)
  (let ((c custom-float-64))
    (cond ((not (zerop (high c)))
           (let* ((length (integer-length (high c)))
                  (diff (- 64 length)))
             (unless (zerop diff)
               (setf (high c)
                     (logior (ash (high c) diff)
                             (ldb (byte diff length)
                                  (low c))))
               (setf (low c)
                     (ash (ldb (byte length 0)(low c)) diff))
               (decf (exponent c) diff))))
          ((not (zerop (low c)))
           (let* ((length (integer-length (low c)))
                  (diff (- 64 length)))
             (unless (zerop diff)
               (setf (high c)
                     (ash (low c) diff))
               (setf (low c) 0)
               (decf (exponent c) (+ 64 diff)))))
          (t
           nil))))
