(cl:in-package #:buoy)

(defun sin-rational-first-quarter-period (angle)
  (if (>= angle (/ +pi-rational+ 4))
      (cos-rational-first-quarter-period (- (/ +pi-rational+ 2) angle))
      (sin-rational-first-eigth-period angle)))

(defun sin-rational-first-half-period (angle)
  (if (>= angle (/ +pi-rational+ 2))
      (sin-rational-first-quarter-period (- +pi-rational+ angle))
      (sin-rational-first-quarter-period angle)))

(defun sin-rational-first-period (angle)
  (if (>= angle +pi-rational+)
      (- (sin-rational-first-half-period (- angle +pi-rational+)))
      (sin-rational-first-half-period angle)))

(defun sin-rational-non-negative (angle)
  (sin-rational-first-period (mod angle (* 2 +pi-rational+))))

(defun sin-rational-arbitrary (angle)
  (if (minusp angle)
      (- (sin-rational-non-negative (- angle)))
      (sin-rational-non-negative angle)))
