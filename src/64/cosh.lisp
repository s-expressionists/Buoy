(cl:in-package #:buoy-core-math-64)

;;; The function cosh(x) is approximated by a minimax polynomial
;;; cosh(x)~1+x^2*P(x^2) for |x|<0.125. For other arguments the
;;; identity cosh(x)=(exp(|x|)+exp(-|x|))/2 is used. For |x|<5 both
;;; exponents are calculated with slightly higher precision than
;;; double. For 5<|x|<36.736801, exp(-|x|) is rather small and is
;;; calculated with double precision but exp(|x|) is calculated with
;;; higher than double precision. For 36.736801<|x|<710.47586
;;; exp(-|x|) becomes too small and only exp(|x|) is calculated,
;;; yielding an accuracy of 106 bits.

(defparameter *cosh-ch-table*
  (make-array
   '(4 2)
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (list (p "0x1.0p-1")
                 (p "-0x1.c7e8db669f624p-111"))   ; degree 2
           (list (p "{0x1.5555555555555p-5")
                 (p "0x1.5555555556135p-59"))     ; degree 4
           (list (p "{0x1.6c16c16c16c17p-10")
                 (p "-0x1.f49f4a6e838f2p-65"))    ; degree 6
           (list (p "0x1.a01a01a01a01ap-16")
                 (p "0x1.a4ffbe15316aap-76")))))) ; degree 8

(defparameter *cosh-cl-table*
  (make-array
   4
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (p "0x1.27e4fb7789f5cp-22")     ; degree 10
           (p "0x1.1eed8eff9089cp-29")     ; degree 12
           (p "0x1.939749ce13dadp-37")     ; degree 14
           (p "0x1.ae9891efb6691p-45"))))) ; degree 16
   
(defparameter *cosh-database*
  (make-array
   '(21 3)
   :initial-contents
   (flet ((p (x) (parse-c-literal x)))
     (list (list (p "0x1.9a5e3cbe1985ep-4")
                 (p "0x1.01492f72f984bp+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.52a11832e847dp-3")
                 (p "0x1.0381e68cac923p+0")
                 (p "0x1.0p-104"))
           (list (p "0x1.bf0305e2c6c37p-3")
                 (p "0x1.061f4c39e16f2p+0")
                 (p "0x1.0p-107"))
           (list (p "0x1.17326ffc09f68p-2")
                 (p "0x1.099318a43ac8p+0")
                 (p "0x1.0p-104"))
           (list (p "0x1.3d27bf16d8bdbp-2")
                 (p "0x1.0c6091056e06ap+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.03923f2b47c07p-1")
                 (p "0x1.219c1989e3373p+0")
                 (p "-0x1.0p-54"))
           (list (p "0x1.a6031cd5f93bap-1")
                 (p "0x1.5bff041b260fep+0")
                 (p "-0x1.0p-107"))
           (list (p "0x1.104b648f113a1p+0")
                 (p "0x1.9efdca62b700ap+0")
                 (p "-0x1.0p-109"))
           (list (p "0x1.1585720f35cd9p+0")
                 (p "0x1.a5bf3acfde4b2p+0")
                 (p "0x1.0p-105"))
           (list (p "0x1.e9cc7ed2e1a7ep+0")
                 (p "0x1.bb0ff220d8eb5p+1")
                 (p "-0x1.0p-53"))
           (list (p "0x1.43180ea854696p+1")
                 (p "0x1.91f1122b6b63ap+2")
                 (p "0x1.0p-102"))
           (list (p "0x1.725811dcf6782p+2")
                 (p "0x1.45ea160ddc71fp+7")
                 (p "-0x1.0p-100"))
           (list (p "0x1.5afd56f7d565bp+3")
                 (p "0x1.8ff8e0ccea7cp+14")
                 (p "0x1.0p-90"))
           (list (p "0x1.759a2ad4c4d56p+3")
                 (p "0x1.cb62eec26bd78p+15")
                 (p "-0x1.0p-92"))
           (list (p "0x1.7fce95ea5c653p+3")
                 (p "0x1.3bf8009648dcp+16")
                 (p "0x1.0p-88"))
           (list (p "0x1.743d5609348acp+4")
                 (p "0x1.7a87a8bb7fa28p+32")
                 (p "-0x1.0p-22"))
           (list (p "0x1.e07e71bfcf06fp+5")
                 (p "0x1.91ec4412c344fp+85")
                 (p "0x1.0p-24"))
           (list (p "0x1.6474c604cc0d7p+6")
                 (p "0x1.7a8f65ad009bdp+127")
                 (p "-0x1.0p+20"))
           (list (p "0x1.54cd1fea7663ap+7")
                 (p "0x1.c90810d354618p+244")
                 (p "0x1.0p+135"))
           (list (p "0x1.2da9e5e6af0bp+8")
                 (p "0x1.27d6fe867d6f6p+434")
                 (p "0x1.0p+329"))
           (list (p "0x1.d6479eba7c971p+8")
                 (p "0x1.62a88613629b6p+677")
                 (p "-0x1.0p+568"))))))

;;; This function does a binary search, but it tests for equality in
;;; each iteration, which doubles the number of tests.  But then, this
;;; function is probably not executed very often.  It also has two
;;; occurrences of (floor (+ a b) 2) which is unnecessary.  There
;;; should be a single occurrence at the beginning of the loop.  Also,
;;; there is no point in assigning to F and then do a local control
;;; transfer.  Might as well return from the function right there.
(defun as-cosh-database (x f)
  (let* ((tt *cosh-database*)
         (a 0)
         (b (1- (array-dimension tt 0)))
         (m (floor (+ a b) 2))
         (ax (abs x)))
    (loop while (<= a b)
          do (cond ((< (aref tt m 0) ax)
                    (setf a (1+ m)))
                   ((= (aref tt m 0) ax)
                    (setf f (+ (aref tt m 1) (aref tt m 2)))
                    (loop-finish))
                   (t
                    (setf b (1- m))))
             (setf m (floor (+ a b) 2)))
    f))

(defun cosh-0<=x<1/4 (x)
  (if (< x #.(sim:dfloat (expt 2 -26)))
      1d0
      (cosh-2^-26<=x<1/4)))

(defun cosh-x>=0 (x)
  (if (< x #.(sim:dfloat 1/4))
      (cosh-0<=x<1/4)
      (cosh-x>=1/4)))

(defun cr-cosh (x)
  (cosh-x>=0 (abs x)))
