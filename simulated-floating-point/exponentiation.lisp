(cl:in-package #:buoy-simulate)

(defun power (x integer)
  (cond ((zerop integer) 1)
        ((evenp integer)
         (power (* x x) (/ integer 2)))
        (t
         (* x (power x (1- integer))))))

(defun rational-exp-with-small-argument (argument)
  (1+ (loop for numerator = argument then (* numerator argument)
            for i from 1 to 20
            for denominator = 1 then (* denominator i)
            sum (/ numerator denominator))))

(defun rational-exp (argument)
  (if (> argument 1)
      (let* ((numerator (numerator argument))
             (numerator-length (integer-length numerator))
             (denominator (denominator argument))
             (denominator-length (integer-length denominator))
             (diff (- numerator-length denominator-length)))
        (power (rational-exp-with-small-argument
                (/ numerator (ash denominator (1+ diff))))
               (ash 1 (1+ diff))))
      (rational-exp-with-small-argument argument)))
