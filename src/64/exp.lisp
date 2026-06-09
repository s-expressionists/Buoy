(cl:in-package #:buoy-core-math-64)

(defun o-poly-dd (xh xl n table)
  (let* ((i (1- n))
         (ch (aref table i 0))
         (cl (aref table i 1)))
    (loop for j downfrom (1- i) to 0
          do (multiple-value-setq (ch cl) (multiply-dd xh xl ch cl))
             (let* ((th (+ ch (aref table i 0)))
                    (tl (+ (- (aref table i 0) th) ch)))
               (setf ch th)
               (incf cl (+ tl (aref table i 1)))))
    (values ch cl)))
