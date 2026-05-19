(cl:in-package #:buoy-simulate)

(defparameter *odd-factors*
  (cons pf:*zero* (loop for i from 3 by 2 to 200
                        collect (pf:pfloat-from-rational (/ (* (1- i) i))))))

;;; This function should be called with a small-ish argument.  It will
;;; converge for any argument, but it might take a long time.  For an
;;; argument equivalent to 3/4, it will take 22 iterations, and for an
;;; argumen equivalent to 3/8 it will take 20 iterations.  Not a big
;;; difference.
(defun pfloat-sine-with-small-argument (pfloat)
  (loop with sum = pf:*zero*
        with square = (pf:* pfloat pfloat)
        for i from 0
        for factor in *odd-factors*
        for term = pfloat then (pf:* (pf:* term square) factor)
        for sum2 = term then (if (oddp i) (pf:- sum term) (pf:+ sum term))
        until (pf:= sum sum2)
        do (setf sum sum2)
        finally (return sum)))

(defun pfloat-sine-with-positive-argument (pfloat)
  (let* ((rational (pf:rational-from-pfloat pfloat))
         (rational-less-than-2-pi (mod rational (* 2 *pi*)))
         (small-pfloat (pf:pfloat-from-rational rational-less-than-2-pi)))
    (pfloat-sine-with-small-argument small-pfloat)))

(defun pfloat-sine (pfloat)
  (cond ((pf:zerop pfloat)
         pf:*zero*)
        ((pf:minusp pfloat)
         (pf:negate (pfloat-sine-with-positive-argument (pf:negate pfloat))))
        (t
         (pfloat-sine-with-positive-argument pfloat))))

(defparameter *even-factors*
  (cons pf:*zero* (loop for i from 2 by 2 to 200
                        collect (pf:pfloat-from-rational (/ (* (1- i) i))))))

(defun pfloat-cosine-with-small-argument (pfloat)
  (loop with sum = pf:*zero*
        with square = (pf:* pfloat pfloat)
        for i from 0
        for factor in *even-factors*
        for term = pf:*one* then (pf:* (pf:* term square) factor)
        for sum2 = term then (if (oddp i) (pf:- sum term) (pf:+ sum term))
        until (pf:= sum sum2)
        do (setf sum sum2)
        finally (return sum)))

(defun pfloat-cosine-with-positive-argument (pfloat)
  (let* ((rational (pf:rational-from-pfloat pfloat))
         (rational-less-than-2-pi (mod rational (* 2 *pi*)))
         (small-pfloat (pf:pfloat-from-rational rational-less-than-2-pi)))
    (pfloat-cosine-with-small-argument small-pfloat)))

(defun pfloat-cosine (pfloat)
  (cond ((pf:zerop pfloat)
         pf:*one*)
        ((pf:minusp pfloat)
         (pfloat-cosine-with-positive-argument (pf:negate pfloat)))
        (t
         (pfloat-cosine-with-positive-argument pfloat))))

(defun rational-sine (rational)
  (let* ((pfloat (pf:pfloat-from-rational rational))
         (pfloat-result (pfloat-sine pfloat)))
    (pf:rational-from-pfloat pfloat-result)))

(defun rational-cosine (rational)
  (let* ((pfloat (pf:pfloat-from-rational rational))
         (pfloat-result (pfloat-cosine pfloat)))
    (pf:rational-from-pfloat pfloat-result)))
