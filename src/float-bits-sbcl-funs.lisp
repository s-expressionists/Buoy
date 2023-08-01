(in-package #:buoy)

(declaim (inline ftrunc))
(defun ftrunc (y) (round-double y :truncate))
(defun unsigned-bits-double-float (x) (unsigned-bits-double-float x))
(defun unsigned-bits-single-float (x) (unsigned-bits-single-float x))
(defun single-float-unsigned-bits (x) (single-float-unsigned-bits x))
(defun single-float-signed-bits (x) (sb-kernel:single-float-bits x))
(defun signed-bits-single-float (x) (sb-kernel:make-single-float x))
(defun double-float-signed-bits (x)
  (sb-kernel:double-float-bits x))
(defun double-float-unsigned-bits (x)
  (ldb (byte 64 0) (sb-kernel:double-float-bits x)))

(declaim (inline signed-bits-double-float))
(defun signed-bits-double-float (x)
  (unsigned-bits-double-float (ldb (byte 64 0) x)))

(defun fma-double (x y z) (fma-double x y z))
