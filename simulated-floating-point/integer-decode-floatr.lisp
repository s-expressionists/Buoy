(cl:in-package #:buoy-simulate)

(defun integer-decode-single-floatr (floatr)
  (if (zerop floatr)
      (values 0 0)
      (let ((numerator (numerator floatr))
            (denominator (denominator floatr)))
        (if (< floatr (/ (ash 1 126)))
            (let* ((length (integer-length denominator))
                   (diff (- 150 length)))
              (values (ash numerator diff) -149))
            (let* ((length (integer-length numerator))
                   (diff (- 24 length))
                   (shifted (ash numerator diff)))
              (if (minusp diff)
                  (values shifted
                          (- diff))
                  (values shifted
                          (- 1 (integer-length denominator) diff))))))))
