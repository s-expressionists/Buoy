(cl:in-package #:buoy-simulate)

(defun decode-floatr (floatr)
  (let ((numerator (numerator floatr))
        (denominator (denominator floatr)))
    (let ((numerator-length (integer-length numerator))
          (denominator-length (integer-length denominator)))
      (let ((difference (- numerator-length denominator-length)))
        (if (minusp difference)
            (setf numerator (ash numerator (- difference)))
            (setf denominator (ash denominator difference)))
        ;; We need to return a first value that is between 0.5
        ;; (inclusive) and 1 (exclusive).  If the numerator is
        ;; strictly less than the denominator, then the numerator and
        ;; the denominator already designates such a number.
        ;; Otherwise, we must increase the value of the denominator.
        (unless (< numerator denominator)
          (setf denominator (ash denominator 1))
          (incf difference))
        (values (/ numerator denominator) difference)))))
