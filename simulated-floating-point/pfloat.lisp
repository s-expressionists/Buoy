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

(defun exponent (pfloat)
  (cdr pfloat))

(defun normalize-pfloat (pfloat)
  (let* ((mantissa (mantissa pfloat))
         (length (integer-length mantissa))
         (diff (cl:- *precision* length)))
    (setf (car pfloat) (ash mantissa diff))
    (decf (cdr pfloat) diff))
  pfloat)

(defun make-pfloat (mantissa exponent)
  (normalize-pfloat (cons mantissa exponent)))

(defun pfloat-from-rational (rational)
  (let ((sign (if (cl:minusp rational) -1 1))
        (mantissa (if (cl:minusp rational) (cl:- rational) rational))
        exponent)
    (let* ((numerator (numerator mantissa))
           (numerator-length (integer-length numerator))
           (denominator (denominator mantissa))
           (denominator-length (integer-length denominator))
           (difference (cl:- numerator-length denominator-length)))
      (setf exponent difference)
      (if (cl:minusp difference)
          ;; Then we shift the numerator by the negative
          ;; difference.
          (setf numerator (ash numerator (cl:- difference)))
          ;; Otherwise, we shift the denominator by the
          ;; difference.
          (setf denominator (ash denominator difference)))
      ;; At this point either the numerator is less than the
      ;; denominator so that the quotient is less than 1, or
      ;; the numerator is greater than or equal to the
      ;; denominator, so that the quotient is greater than or
      ;; equal to 1.  We want the first case.
      (unless (cl:< numerator denominator)
        (incf exponent)
        (setf denominator (ash denominator 1)))
      ;; Now, we shift the numerator by *PRECISION* positions to get
      ;; something that should be an integer in the PFLOAT
      ;; representation.
      (setf numerator (ash numerator *precision*))
      (decf exponent *precision*)
      (make-pfloat (cl:* sign (round (cl:/ numerator denominator))) exponent))))

(defparameter *zero*
  (cons 0 0))

(defun zerop (pfloat)
  (equal pfloat *zero*))

(defparameter *one*
  (pfloat-from-rational 1))

(defparameter *two*
  (pfloat-from-rational 2))

(defun * (pfloat1 pfloat2)
  (let ((mantissa (cl:* (mantissa pfloat1)
                        (mantissa pfloat2)))
        (exponent (cl:+ (exponent pfloat1)
                        (exponent pfloat2))))
    (if (cl:zerop mantissa)
        *zero*
        (normalize-pfloat (make-pfloat mantissa exponent)))))

(defun / (pfloat1 pfloat2)
  (cond ((cl:zerop (mantissa pfloat2))
         (error 'division-by-zero))
        ((cl:zerop (mantissa pfloat1))
         *zero*)
        (t
         (let ((mantissa (round (cl:/ (ash (mantissa pfloat1) *precision*)
                                      (mantissa pfloat2))))
               (exponent (cl:- (exponent pfloat1)
                               (exponent pfloat2)
                               *precision*)))
           (make-pfloat mantissa exponent)))))

(defun = (pfloat1 pfloat2)
  (and (cl:= (mantissa pfloat1) (mantissa pfloat2))
       (cl:= (exponent pfloat1) (exponent pfloat2))))

(defun < (pfloat1 pfloat2)
  (if (cl:minusp (mantissa pfloat1))
      (or (not (cl:minusp (mantissa pfloat2)))
          (cl:> (exponent pfloat1)
                (exponent pfloat2))
          (and (cl:= (exponent pfloat1)
                     (exponent pfloat2))
               (cl:< (mantissa pfloat1)
                     (mantissa pfloat2))))
      (and (not (cl:minusp (mantissa pfloat2)))
           (or (cl:< (exponent pfloat1)
                     (exponent pfloat2))
               (and (cl:= (exponent pfloat1)
                          (exponent pfloat2))
                    (cl:< (mantissa pfloat1)
                          (mantissa pfloat2)))))))

(defun minusp (pfloat)
  (cl:minusp (mantissa pfloat)))

(defun negate (pfloat)
  (make-pfloat (cl:- (mantissa pfloat))
               (exponent pfloat)))

(defun + (pfloat1 pfloat2)
  (when (cl:< (exponent pfloat1)
              (exponent pfloat2))
    (rotatef pfloat1 pfloat2))
  (cond ((zerop pfloat1)
         pfloat2)
        ((zerop pfloat2)
         pfloat1)
        (t
         (let* ((mantissa1 (mantissa pfloat1))
                (exponent1 (exponent pfloat1))
                (mantissa2 (mantissa pfloat2))
                (exponent2 (exponent pfloat2))
                (diff (cl:- exponent1 exponent2))
                (resulting-mantissa (cl:+ mantissa1 (ash mantissa2 (cl:- diff)))))
           (if (cl:zerop resulting-mantissa)
               *zero*
               (make-pfloat resulting-mantissa exponent1))))))

(defun - (pfloat1 pfloat2)
  (+ pfloat1 (negate pfloat2)))

(defun rational-from-pfloat (pfloat)
  (cl:* (mantissa pfloat)
        (expt 2 (exponent pfloat))))

;;; MANTISSA-WIDTH is the number of bits used to represent the
;;; mantissa in the binary representation of the floating-point
;;; number.  Here we are not particularly interested in how IEEE
;;; floats are really represented in a machine word, but more in the
;;; values that are represented.
(defun restrict-to-ieee-precision (pfloat mantissa-width exponent-width)
  (let ((ieee-exponent (cl:+ (exponent pfloat) *precision* -1)))
    ;; IEEE exponent is the exponent we would get if we turned the
    ;; mantissa of the pfloat into a number that is greater than or
    ;; equal to 1 but less than 2.
    (let ((max-exponent (ash 1 (1- exponent-width))))
      ;; If a number is greater than or equal to (EXPT 2 MAX-EXPONENT),
      ;; then we have arithmetic overflow.
      (when (cl:>= ieee-exponent max-exponent)
        (if (minusp pfloat)
            (error 'floating-point-underflow)
            (error 'floating-point-overflow))))
    (let* ((exponent-of-least-positive-normal-float
             (cl:- 2 (ash 1 (1- exponent-width))))
           (exponent-of-least-positive-float
             (cl:- exponent-of-least-positive-normal-float
                   mantissa-width)))
      ;; The least positive normal float is (EXPT 2
      ;; EXPONENT-OF-LEAST-POSITIVE-NORMAL-FLOAT), and the least
      ;; positive float is (EXPT 2 EXPONENT-OF-LEAST-POSITIVE-FLOAT).
      (if (cl:< ieee-exponent exponent-of-least-positive-float)
          *zero*
          (let ((desired-precision (cl:+ ieee-exponent
                                         exponent-of-least-positive-float
                                         1)))
            (make-pfloat (round (cl:/ (mantissa pfloat) (ash 1 desired-precision)))
                         (cl:+ (exponent pfloat) desired-precision)))))))

(defun restrict-to-ieee-single (pfloat)
  (restrict-to-ieee-precision pfloat 23 8))

(defun restrict-to-ieee-double (pfloat)
  (restrict-to-ieee-precision pfloat 52 11))
