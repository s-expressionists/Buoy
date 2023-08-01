(cl:in-package #:asdf-user)

(defsystem :buoy
  :serial t
  :depends-on ("float-features")
  :components ((:file "src/package")
	       (:file "src/fma")
	       ;; this needs to go in its own module because vops have some unfortunate interactions with file-compilation
	       #+sbcl (:module ""
		       :components ((:file "src/float-bits-sbcl")))
	       #+sbcl (:file "src/float-bits-sbcl-funs")
	       #-sbcl (:file "src/float-bits")
               (:file "src/basics")
	       (:file "src/scale-float-defun")
	       (:file "src/sin-cos-table")
	       (:file "src/sin-cos-single-defun")
	       ))
