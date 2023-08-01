(in-package #:sb-vm)

(defknown buoy::single-float-unsigned-bits (single-float) (unsigned-byte 32)
  (movable foldable flushable always-translatable))
(defknown buoy::unsigned-bits-single-float ((unsigned-byte 32)) (single-float)
  (movable foldable flushable always-translatable))
(defknown buoy::unsigned-bits-double-float ((unsigned-byte 64)) (double-float)
  (movable foldable flushable always-translatable))
;(defknown buoy::signed-bits-double-float ((signed-byte 64)) (double-float)
;  (movable foldable flushable always-translatable))

#+x86-64
(define-vop (buoy::single-float-unsigned-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (unsigned-reg)))
  (:arg-types single-float)
  (:result-types unsigned-num)
  (:translate buoy::single-float-unsigned-bits)
  (:policy :fast-safe)
  (:generator 4
     (sc-case float
       (single-reg
        (inst movd bits float))
       (single-stack ; c.f. ea-for-sf-stack
        (inst mov ;'(:dword :qword)
              bits (ea (frame-byte-offset (tn-offset float)) rbp-tn)))
       (descriptor-reg
        (move bits float)
        (inst shr bits 32)))))
#+x86-64
(define-vop (buoy::unsigned-bits-single-float)
  (:args (bits :scs (unsigned-reg) :target res
               :load-if (not (or (and (sc-is bits unsigned-stack)
                                      (sc-is res single-reg))
                                 (and (sc-is bits unsigned-stack)
                                      (sc-is res single-stack)
                                      (location= bits res))))))
  (:results (res :scs (single-reg single-stack)))
  (:arg-types unsigned-num)
  (:result-types single-float)
  (:translate buoy::unsigned-bits-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case res
       (single-stack
        (sc-case bits
          (unsigned-reg
           (inst mov res bits))
          (unsigned-stack
           (aver (location= bits res)))))
       (single-reg
        (sc-case bits
          (unsigned-reg
           (inst movd res bits))
          (unsigned-stack
           (inst movss res
                 (ea (frame-byte-offset (tn-offset bits)) rbp-tn))))))))
#+x86-64
(define-vop (buoy::unsigned-bits-double-float)
  (:args (bits :scs (unsigned-reg) :target res
               :load-if (not (or (and (sc-is bits unsigned-stack)
                                      (sc-is res double-reg))
                                 (and (sc-is bits unsigned-stack)
                                      (sc-is res double-stack)
                                      (location= bits res))))))
  (:results (res :scs (double-reg double-stack)))
  (:arg-types unsigned-num)
  (:result-types double-float)
  (:translate buoy::unsigned-bits-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case res
       (double-stack
        (sc-case bits
          (unsigned-reg
           (inst mov res bits))
          (unsigned-stack
           (aver (location= bits res)))))
       (double-reg
        (sc-case bits
          (unsigned-reg
           (inst movq res bits))
          (unsigned-stack
           (inst movsd res
                 (ea (frame-byte-offset (tn-offset bits)) rbp-tn))))))))
#+arm64
(define-vop (buoy::single-float-unsigned-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (unsigned-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits unsigned-stack)))))
  (:arg-types single-float)
  (:result-types unsigned-num)
  (:translate buoy::single-float-unsigned-bits)
  (:policy :fast-safe)         
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (unsigned-reg
       (sc-case float
         (single-reg
          (inst fmov bits float))
         (single-stack
          (inst ldr (32-bit-reg bits)
                (@ (current-nfp-tn vop)
                   (load-store-offset (ash (tn-offset float) 3)))))
         (descriptor-reg
          (inst lsr bits float 32))))
      (unsigned-stack
       (sc-case float
         (single-reg
          (storew (32-bit-reg float) (current-nfp-tn vop) (tn-offset bits)))
         ((single-stack descriptor-reg)
          ;; Fun and games: This also affects PPC, silently.
          ;; Hopefully it's a non-issue, but I'd rather have the
          ;; explicit error than a silent miscompilation. 
          (bug "Unable to extract single-float bits from ~S to ~S" float bits)))))))

(defknown buoy::round-double (double-float (member :round :floor :ceiling :truncate))
    double-float
    (foldable flushable movable always-translatable))
(define-vop ()
  (:translate buoy::round-double)
  (:policy :fast-safe)
  (:args (x :scs (double-reg) :target r))
  (:arg-types double-float (:constant symbol))
  (:info mode)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:generator 2
              (unless (location= r x) 
                (inst xorpd r r))
              (inst roundsd r x
                    (logior #b1000
                            (ecase mode
                              (:round 0)
                              (:floor 1)
                              (:ceiling 2)
                              (:truncate 3))))))

(defknown buoy::fma-double (double-float double-float double-float) double-float
    (movable foldable flushable always-translatable))

#+arm64
(define-vop (buoy::fma-double)
  (:translate buoy::fma-double)
  (:policy :fast-safe)
  (:args (accumulator :scs (double-reg))
         (multiplier :scs (double-reg))
         (multiplicand :scs (double-reg)))
  (:arg-types double-float double-float double-float)
  (:results (res :scs (double-reg)))
  (:result-types double-float)
  (:generator 2
              (inst fmadd res multiplier multiplicand accumulator)))

#+x86-64
(define-vop (buoy::fma-double)
  (:translate buoy::fma-double)
  (:policy :fast-safe)
  (:args (accumulator :scs (double-reg) :target res)
         (multiplier :scs (double-reg))
         (multiplicand :scs (double-reg)))
  (:arg-types double-float double-float double-float)
  (:results (res :scs (double-reg)))
  (:result-types double-float)
  (:generator 2
              (cond
                ((location= res multiplier)
                 (inst vfmadd213sd multiplier multiplicand accumulator))
                ((location= res multiplicand)
                 (inst vfmadd213sd multiplicand multiplier accumulator))
                (t
                 (unless (location= res accumulator)
                   (move res accumulator))
                 (inst vfmadd231sd res multiplier multiplicand)))))
