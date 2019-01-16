;;; Unbounded Spigot algorithm for digits of pi
;;; http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf
;;;
;;; Implements the algorithm on page 7 in a more Lispy way
;;;
;;; To use, call (PRINT-DIGITS N) where N is the number of digits to print.

(defpackage #:pidigits-fast
  (:use #:cl))

(in-package #:pidigits-fast)

#+sbcl
(require :sb-gmp)

(declaim (optimize (speed 3) (debug 0) (space 0) (safety 0)))

;;; type utils

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

;;; spigot algorithm

(defstruct (lft
             (:constructor lft (zq zr zs zt))
             (:conc-name ""))
  (zq 0 :type integer)
  (zr 0 :type integer)
  (zs 0 :type integer)
  (zt 0 :type integer))

(declaim (ftype (function (lft lft) lft) comp))

(defun comp (m1 m2)
  (let ((zq (zq m1)) (zr (zr m1)) (zt (zt m1))
        (zu (zq m2)) (zv (zr m2)) (zx (zt m2)))
    (lft (* zq zu)
         (+ (* zq zv) (* zr zx))
         0
         (* zt zx))))

(declaim (ftype (function (lft (integer 3 4)) (integer 0 9)) extr))

(defun extr (m x)
  (values (floor (+ (* x (zq m)) (zr m))
                 (zt m))))

(defun lft-generator ()
  (let ((k 0))
    (declare (type positive-fixnum k))
    (lambda ()
      (incf k)
      (lft k (+ (* 4 k) 2) 0 (+ (* 2 k) 1)))))

(defun pi-generator ()
  (let ((z (lft 1 0 0 1))
        (lft-gen (lft-generator)))
    (declare (type lft z))
    (lambda ()
      (loop
         (let ((y (extr z 3)))
           (if (= y (extr z 4))
               (progn
                 (setf z (comp (lft 10 (* -10 y) 0 1) z))
                 (return y))
               (setf z (comp z (funcall lft-gen)))))))))

(defun print-digits (n &optional (out *standard-output*))
  (declare (type stream out))
  (let ((generator (pi-generator)))
    (labels ((print-digit (i)
               (declare (type positive-fixnum i n))
               (when (<= i n)
                 (princ (funcall generator) out)
                 (cond
                   ((zerop (mod i 10))
                    (format out "~A:~D~%" #\Tab i))
                   ((= i n)
                    (loop :repeat (- 10 (mod i 10))
                       :do (princ #\Space out))
                    (format out "~A:~D~%" #\Tab i)))
                 (print-digit (1+ i)))))
      (print-digit 1))))
