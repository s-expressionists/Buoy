(cl:in-package #:buoy-simulate)

(defun binary64-from-binary32 (binary32)
  (binary64-from-rational (value binary32)))

(defun binary32-from-binary64 (binary64)
  (binary32-from-rational (value binary32)))
