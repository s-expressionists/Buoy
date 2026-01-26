(cl:in-package #:buoy-simulate)

;;; We define a FLOATR to be a rational value that is the exact value
;;; of some floating-point number.  A CANDIDATE FLOATR is a rational
;;; value that, when rounded, becomes a floatr. 

(defun most-positive-normal-exponent (exponent-width)
  (1- (ash 1 (1- exponent-width))))

;;; The least positive infinite candidate floatr is a rational that is
;;; the representation of a floating point value that can be
;;; constructed as the maximum exponent allowed for normal
;;; floating-point values, and with a mantissa 1.111... where the
;;; number of 1s following the period is one more than what the
;;; representation allows.  If an attempt is made to round this
;;; mantissa to an even value, then the result will be larger than the
;;; largest normal floater.  A value only slightly less than this will
;;; round to the largest positive normal floatr.
(defun least-positive-infinite-candidate-floatr
    (exponent-width mantissa-width)
  ;; We construction an integer that contains a string of N 1s, where
  ;; N is the width of the mantissa plus 2 (plus 1 for the bit that is
  ;; not represented, and 1 for the additional bit at the end).
  (let ((ones (1- (ash 1 (+ mantissa-width 2)))))
    ;; To get the real mantissa from this integer, we must divide it
    ;; with (EXPT 2 M) such that the result is a number greater than 1
    ;; but less than 2.  For that to work, M must be equal to N-1
    ;; which is the width of the mantissa plus 1.
    (let ((mantissa (/ ones (ash 1 (1+ mantissa-width)))))
      ;; We must now multiply the mantissa and the most positive
      ;; normal exponent to get the final result
      (* mantissa (most-positive-normal-exponent exponent-width)))))

;;; The least positive normal candidate floatr is the smallest
;;; rational number that, when rounded, becomes a normal floatr.  It
;;; is the representation of a floating-point value that has the
;;; smallest possible exponent, and a mantissa that can be written as
;;; 0.111... where the number of 1s is one more than the width of the
;;; represented mantissa.  This mantissa rounds to the integer 1,
;;; which then becomes the smallest positive normal floatr.
(defun least-positive-normal-candidate-floatr (exponent-width mantissa-width)
  ;; We construct an integer that contains a string of N 1s, where N
  ;; is the width of the mantissa plus 1.
  (let ((ones (1- (ash 1 (1+ mantissa-width)))))
    ;; To get the real mantissa from this integer, we must divide it
    ;; with (EXPT 2 M) such that the result is a number greater than
    ;; 1/2 but less than 1.  For that to work, M must be equal to N.
    ;; which is the width of the mantissa plus 1.
    (let ((mantissa (/ ones (ash 1 (1+ mantissa-width)))))
      ;; We must now multiply the mantissa and the least positive
      ;; normal exponent to get the final result.
      (* mantissa (least-positive-normal-exponent exponent-width)))))

(defun floatr-from-rational (rational exponent-width mantissa-width)
  (let ((sign (if (minusp rational) -1 1))
        (mantissa (if (minusp rational) (- rational) rational))
        exponent)
    (cond ((>= rational
               (least-positive-infinite-candidate-floatr
                exponent-width mantissa-width))
           ;; We return NIL to indicate infinity
           nil)
          ((>= rational
               (least-positive-normal-candidate-floatr
                exponent-width mantissa-width))
           ;; We have a normal floatr.
           (let* ((numerator (numerator mantissa))
                  (numerator-length (integer-length numerator))
                  (denominator (denominator mantissa))
                  (denominator-length (integer-length denominator))
                  (difference (- numerator-length denominator-length)))
             (setf exponent difference)
             (if (minusp difference)
                 ;; Then we shift the numerator by the negative
                 ;; difference.
                 (setf numerator (ash numerator (- difference)))
                 ;; Otherwise, we shift the denominator by the
                 ;; difference.
                 (setf denominator (ash denominator difference)))
             ;; At this point either the numerator is less than the
             ;; denominator so that the quotient is less than 1, or
             ;; the numerator is greater than or equal to the
             ;; denominator, so that the quotient is greater than or
             ;; equal to 1.  In the second case, we have something
             ;; that can be used as the mantissa of the float but in
             ;; the first case, we must multiply the denominator by 2.
             (when (< numerator denominator)
               (decf exponent)
               (setf numerator (ash numerator 1)))
             ;; Now, we shift the numerator by MANTISSA-WIDTH
             ;; positions to get something that should be an integer
             ;; in the floating-point representation.
             (setf numerator (ash numerator mantissa-width))
             (decf exponent mantissa-width)
             (* sign
                (if (minusp exponent)
                    (/ (ash 1 (- exponent)))
                    (ash 1 exponent))
                (round (/ numerator denominator)))))
          (t t))))


           
        
    

; LocalWords:  floatr
