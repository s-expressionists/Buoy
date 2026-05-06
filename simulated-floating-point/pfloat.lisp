(cl:in-package #:buoy-simulate)

;;; A PFLOAT is a CONS cell where the CAR contains an integer with an
;;; integer length of exactly *FLOAT-PRECISION*, and the CDR contains
;;; an exponent.  The PFLOAT P represents the number (* (CAR P) (EXPT
;;; 2 (CDR P))).  Ether the CAR or the CDR can be either negative or
;;; non-negative.  The purpose of PFLOATs is to speed up calculations
;;; so that generating tables for Buoy is faster.

;;; We want the precision to be fairly good.
(defparameter *pfloat-precision* 200)

(defun pfloat-mantissa (pfloat)
  (car pfloat))

(defun (setf pfloat-mantissa) (mantissa pfloat)
  (setf (car pfloat) mantissa))

(defun pfloat-exponent (pfloat)
  (cdr pfloat))

(defun (setf pfloat-exponent) (exponent pfloat)
  (setf (cdr pfloat) exponent))

(defun normalize-pfloat (pfloat)
  (let* ((mantissa (pfloat-mantissa pfloat))
         (length (integer-length mantissa))
         (diff (- *pfloat-precision* length)))
    (setf (pfloat-mantissa pfloat) (ash mantissa diff))
    (incf (pfloat-exponent pfloat) diff))
  pfloat)

(defun make-pfloat (mantissa exponent)
  (normalize-pfloat (cons mantissa exponent)))
  
(defun multiply-pfloat (pfloat1 pfloat2)
  (let ((mantissa (* (pfloat-mantissa pfloat1)
                     (pfloat-mantissa pfloat2)))
        (exponent (+ (pfloat-exponent pfloat1)
                     (pfloat-exponent pfloat2))))
    (normalize-pfloat (make-pfloat mantissa exponent))))

(defun rational-from-pfloat (pfloat)
  (* (pfloat-mantissa pfloat)
     (expt 2 (pfloat-exponent pfloat))))

(defun double-float-from-pfloat (pfloat)
  (dfloat (rational-from-pfloat pfloat)))
