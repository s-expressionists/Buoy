(declaim (inline single-float-unsigned-bits
                 unsigned-bits-single-float
                 unsigned-bits-double-float))
(defun single-float-unsigned-bits (x) (float-features:single-float-bits x))
(defun unsigned-bits-single-float (x) (float-features:bits-single-float x))
(defun unsigned-bits-double-float (x) (float-features:bits-double-float x))

(declaim (inline signed-bits-double-float))
(defun signed-bits-double-float (x)
  (unsigned-bits-double-float (ldb (byte 64 0) x)))

(declaim (inline single-float-signed-bits signed-bits-single-float))
(defun single-float-signed-bits (x)
  (let ((x (float-features:single-float-bits x)))
    (logior x (- (mask-field (byte 1 31) x)))))
(defun signed-bits-single-float (x) (float-features:bits-single-float (ldb (byte 32 0) x)))

(declaim (inline double-float-unsigned-bits double-float-signed-bits))
(defun double-float-signed-bits (x)
  (let ((x (float-features:double-float-bits x)))
    (logior x (- (mask-field (byte 1 63) x)))))
(defun double-float-unsigned-bits (x)
  (float-features:double-float-bits x))

(declaim (inline ftrunc))
(declaim (ftype (function (double-float) double-float) ftrunc))

(defun ftrunc (y) (ftruncate y))
