(cl:in-package #:buoy-simulate)

(defun floatr-from-rational (rational exponent-width mantissa-width)
  (let ((sign (if (minusp rational) -1 1))
        (mantissa (if (minusp rational) (- rational) rational))
        exponent)
    (cond ((>= mantissa
               (least-positive-infinite-candidate-floatr
                exponent-width mantissa-width))
           ;; We return NIL to indicate infinity
           nil)
          ((>= mantissa
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
          (t
           (let* ((exponent
                    (+ mantissa-width
                       (1- (most-positive-normal-exponent exponent-width))))
                  (factor (ash 1 exponent)))
             (/ (round (* rational factor)) factor))))))

(defun floatr32-from-rational (rational)
  (floatr-from-rational rational 8 23))

(defun floatr64-from-rational (rational)
  (floatr-from-rational rational 11 52))
