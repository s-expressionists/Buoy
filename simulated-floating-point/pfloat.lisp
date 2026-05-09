(cl:in-package #:buoy-pfloat)

;;; A PFLOAT is a CONS cell where the CAR contains an integer with an
;;; integer length of exactly *FLOAT-PRECISION*, and the CDR contains
;;; an exponent.  The PFLOAT P represents the number (* (CAR P) (EXPT
;;; 2 (CDR P))).  Ether the CAR or the CDR can be either negative or
;;; non-negative.  The purpose of PFLOATs is to speed up calculations
;;; so that generating tables for Buoy is faster.

;;; We want the precision to be fairly good.
(defparameter *precision* 200)

(defun mantissa (pfloat)
  (car pfloat))

(defun (setf mantissa) (mantissa pfloat)
  (setf (car pfloat) mantissa))

(defun exponent (pfloat)
  (cdr pfloat))

(defun (setf exponent) (exponent pfloat)
  (setf (cdr pfloat) exponent))

(defun normalize-pfloat (pfloat)
  (let* ((mantissa (mantissa pfloat))
         (length (integer-length mantissa))
         (diff (- *precision* length)))
    (setf (mantissa pfloat) (ash mantissa diff))
    (decf (exponent pfloat) diff))
  pfloat)

(defun make-pfloat (mantissa exponent)
  (normalize-pfloat (cons mantissa exponent)))

(defun * (pfloat1 pfloat2)
  (let ((mantissa (cl:* (mantissa pfloat1)
                        (mantissa pfloat2)))
        (exponent (cl:+ (exponent pfloat1)
                        (exponent pfloat2))))
    (normalize-pfloat (make-pfloat mantissa exponent))))

(defun / (pfloat1 pfloat2)
  (let ((mantissa (round (cl:/ (ash (mantissa pfloat1) *precision*)
                               (mantissa pfloat2))))
        (exponent (cl:- (exponent pfloat1)
                        (exponent pfloat2)
                        *precision*)))
    (make-pfloat mantissa exponent)))

(defun < (pfloat1 pfloat2)
  (if (minusp (mantissa pfloat1))
      (or (not (minusp (mantissa pfloat2)))
          (cl:> (exponent pfloat1)
                (exponent pfloat2))
          (and (cl:= (exponent pfloat1)
                     (exponent pfloat2))
               (cl:< (mantissa pfloat1)
                     (mantissa pfloat2))))
      (and (not (minusp (mantissa pfloat2)))
           (or (cl:< (exponent pfloat1)
                     (exponent pfloat2))
               (and (cl:= (exponent pfloat1)
                          (exponent pfloat2))
                    (cl:< (mantissa pfloat1)
                          (mantissa pfloat2)))))))

(defun minusp (pfloat)
  (minusp (mantissa pfloat)))

(defun negate (pfloat)
  (make-pfloat (- (mantissa pfloat))
               (exponent pfloat)))

(defun + (pfloat1 pfloat2)
  (when (cl:< (exponent pfloat1)
              (exponent pfloat2))
    (rotatef pfloat1 pfloat2))
  (let* ((mantissa1 (mantissa pfloat1))
         (exponent1 (exponent pfloat1))
         (mantissa2 (mantissa pfloat2))
         (exponent2 (exponent pfloat2))
         (diff (cl:- exponent1 exponent2)))
    (make-pfloat (cl:+ mantissa1 (ash mantissa2 (cl:- diff)))
                 exponent1)))

(defun rational-from-pfloat (pfloat)
  (cl:* (mantissa pfloat)
        (expt 2 (exponent pfloat))))

(defun double-float-from-pfloat (pfloat)
  (dfloat (rational-from-pfloat pfloat)))

(defun pfloat-from-rational (rational)
  (let ((floatr (floatr-from-rational rational 12 200)))
    (make-pfloat (numerator floatr)
                 (cl:- (1- (integer-length (denominator floatr)))))))
