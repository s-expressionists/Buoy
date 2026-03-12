(cl:in-package #:buoy-simulate)

(defparameter *sine-iteration-count* 20)

;;; This function is called when the argument is less than around 4/10
;;; which is less than around (/ pi 8).
(defun rational-sine-small-argument (argument)
  (loop for sign = 1 then (- sign)
        for n from 0 to *sine-iteration-count*
        for 2n = (* n 2)
        for 2n+1! = 1 then (* 2n+1! 2n (1+ 2n))
        for x = argument then (* x argument argument)
        sum (* sign (/ x 2n+1!))))

;;; This function is called when the argument is less than around 4/10
;;; which is less than around (/ pi 8).
(defun rational-cosine-small-argument (argument)
  (loop for sign = 1 then (- sign)
        for n from 0 to *sine-iteration-count*
        for 2n = (* n 2)
        for 2n! = 1 then (* 2n! (1- 2n) 2n)
        for x = 1 then (* x argument argument)
        sum (* sign (/ x 2n!))))

;;; This function is called when the argument is less than or equal to
;;; (/ pi 4).
(defun rational-sine-small-ish-argument (argument)
  (if (< argument 4/10)
      (rational-sine-small-argument argument)
      (let ((small-argument (/ argument 2)))
        (* 2
           (rational-sine-small-argument small-argument)
           (rational-cosine-small-argument small-argument)))))

;;; This function is called when the argument is less than or equal to
;;; (/ pi 4).
(defun rational-cosine-small-ish-argument (argument)
  (if (< argument 4/10)
      (rational-cosine-small-argument argument)
      (let* ((small-argument (/ argument 2))
             (sine (rational-sine-small-argument small-argument)))
        (- 1 (* sine sine)))))
