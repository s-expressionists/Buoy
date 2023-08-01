(cl:in-package #:buoy)

;;; scale-float: returns (* float (expt 2 integer)), correctly rounded.
;;; This is commonly implemented (both in CL and in C) by manual
;;; twiddling of exponent bits.  This is annoying to do, and requires
;;; manual handling of overflow and denormals.  Nicer, and faster, is
;;; simply to multiply.
;;; But there is a problem: the range of a single float's exponent is
;;; a bit more than 2^8, but the maximum magnitude of that exponent is
;;; about 2^7, so there are meaningful values of INTEGER for which
;;; (expt 2 integer) is not exactly representable.  To get around
;;; this, we multiply multiple times when INTEGER is too large.

;;; But could this change the result?  The only case in which we lose
;;; bits will be if we generate a denormal number from a normal one.
;;; If we round twice, we may get an incorrect result (one example
;;; with round to nearest, bias towards even, is 0 10 10 -> 0 10 -> 0
;;; (the correct result is 1)).  So we must make sure to round only
;;; once; we do this by performing one 'partial-width' multiplication,
;;; followed by some number of 'full-width' multiplications; the
;;; full-width multiplications are by (expt 2 -126) or (expt 2 127)
;;; (for single floats, and the analogous values for doubles); since
;;; 126 is more than the number of bits in a significand, this
;;; guarantees we only round once, and automatically accord with the
;;; current rounding mode.

;;; (In the case when we multiply 3 times, we actually do one
;;; full-width multiply, then a partial one, then another full-width
;;; one, for better scheduling.)

(declaim (ftype (function (single-float (integer *  277)) single-float) scale-single-float)
         (ftype (function (double-float (integer * 2098)) double-float) scale-double-float))
(defun scale-single-float (float integer)
  ;; can (expt 2 integer) be exactly represented in a normalised float?  If so, just multiply once
  (if (and (typep integer 'fixnum) (<= -126 integer 127))
      (* float (u2s (ash (+ 127 integer) 23)))
      (let* ((base-exp (if (minusp integer) -126 127))
             (base-mul (u2s (ash (+ 127 base-exp) 23))))
        (if (<= -252 integer 254)
            (let* ((exp (- integer base-exp))
                   (float (* float (u2s (ash (+ 127 exp) 23))))
                   (float (* float base-mul))
                   )
              float)
            ;; need 3 multipliers :\
            (let* ((float (* float base-mul))
                   ;; exp can be arbitrarily small; cap it.
                   ;; If it's that small, the result will definitely be zero so w/e.
                   ;; Single float exponent range is 277; we've handled up to 252, and
                   ;; 25=277-252, so 26 is enough to zero most-positive-single-float.
                   ;; Choose the value of smallest magnitude in order to maximise the chance
                   ;; it fits into an immediate on any given architecture.
                   (exp (max -26 (- integer base-exp base-exp)))
                   (float (* float (u2s (ash (+ 127 exp) 23))))
                   (float (* float base-mul)))
              float)))))
(defun scale-double-float (float integer)
  ;; can (expt 2 integer) be exactly represented in a normalised float?  If so, just multiply once
  (if (<= -1022 integer 1023)
      (* float (float-features:bits-double-float (ash (+ 1023 integer) 52)))
      (let* ((base-exp (if (minusp integer) -1022 1023))
             (base-mul (float-features:bits-double-float (ash (+ 1023 base-exp) 52))))
        (if (<= -2044 integer 2046)
            (let* ((exp (- integer base-exp))
                   (float (* float (float-features:bits-double-float (ash (+ 1023 exp) 52))))
                   (float (* float base-mul)))
              float)
            ;; need 3 multipliers :\
            (let* ((float (* float base-mul))
                   ;; exp can be arbitrarily small; cap it.
                   ;; If it's that small, the result will definitely be zero so w/e.
                   ;; Double float exponent range is 2098; we've handled up to 2044, and
                   ;; 54=2098-2044, so 55 is enough to zero most-positive-double-float.
                   ;; Choose the value of smallest magnitude in order to maximise the chance
                   ;; it fits into an immediate on any given architecture.
                   (exp (max -55 (- integer base-exp base-exp)))
                   (float (* float (float-features:bits-double-float (ash (+ 1023 exp) 52))))
                   (float (* float base-mul)))
              float)))))
