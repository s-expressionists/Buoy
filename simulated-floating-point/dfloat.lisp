(cl:in-package #:buoy-simulate)

;;; The Common Lisp implementation may not have an accurate FLOAT
;;; function.  So we defiine our own:
(defun dfloat (rational)
  (if (zerop rational)
      #.(quaviver:bits-float-form 'double-float 0)
      (let ((sign (if (minusp rational) 1 0))
            (absolute (abs rational))
            exponent)
        (let* ((numerator (numerator absolute))
               (numerator-length (integer-length numerator))
               (denominator (denominator absolute))
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
          ;; that can be used as the absolute of the float but in
          ;; the first case, we must multiply the denominator by 2.
          (when (< numerator denominator)
            (decf exponent)
            (setf numerator (ash numerator 1)))
          (cond ((> exponent 1023)
                 (error 'floating-point-overflow))
                ((> exponent -1023)
                 ;; We have a normal float.  Now, we shift the
                 ;; numerator by 52 positions to get something that
                 ;; should be an integer in the floating-point
                 ;; representation.
                 (setf numerator (ash numerator 52))
                 (let ((mantissa (round (/ numerator denominator))))
                   #.(quaviver:bits-float-form
                      'double-float
                      '(logior (ldb (byte 52 0) mantissa)
                               (ash (+ 1023 exponent) 52)
                        (ash sign 63)))))
                ((> exponent (- -1023 53))
                 ;; We have a subnormal float.
                 (setf numerator
                       (ash numerator (- 51 1023 exponent)))
                 (let ((mantissa (round (/ numerator denominator))))
                   #.(quaviver:bits-float-form
                      'double-float
                      '(logior mantissa
                               (ash 1 52)
                               (ash sign 63)))))
                (t
                 (error 'floating-point-underflow)))))))
