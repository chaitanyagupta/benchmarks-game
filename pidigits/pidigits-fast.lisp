;;; Unbounded Spigot algorithm for digits of pi
;;; http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf
;;;
;;; Implements the algorithm on page 7 in a more Lispy way
;;;
;;; On SBCL, (REQUIRE :SB-GMP) to use the gmplib for better bignum performance.
;;;
;;; To use, call (PRINT-DIGITS N) where N is the number of digits to print.

(defpackage #:pidigits-fast
  (:use #:cl))

(in-package #:pidigits-fast)

(declaim (optimize (speed 3) (debug 0) (space 0) (safety 0)))

;;; spigot algorith

(defstruct (lft
             (:constructor lft (zq zr zs zt))
             (:conc-name ""))
  (zq 0 :type integer)
  (zr 0 :type integer)
  (zs 0 :type integer)
  (zt 0 :type integer))

(declaim (ftype (function (lft lft) lft) comp))

(defun comp (m1 m2)
  (declare (type lft m1 m2))
  (let ((zq (zq m1)) (zr (zr m1)) (zt (zt m1))
        (zu (zq m2)) (zv (zr m2)) (zx (zt m2)))
    (lft (* zq zu)
         (+ (* zq zv) (* zr zx))
         0
         (* zt zx))))

(declaim (ftype (function (lft integer) (integer 0 9)) extr))

(defun extr (m x)
  (declare (type lft m))
  (declare (type integer x))
  (floor (+ (* x (zq m)) (zr m))
         (zt m)))

(defun pi-generator ()
  (let ((k 1)
        (z (lft 1 0 0 1)))
    (flet ((next-lft ()
             (prog1
                 (lft k (+ (* 4 k) 2) 0 (+ (* 2 k) 1))
               (incf k))))
      (lambda ()
        (loop
           (let ((y (extr z 3)))
             (if (= y (extr z 4))
                 (progn
                   (setf z (comp (lft 10 (* -10 y) 0 1) z))
                   (return y))
                 (setf z (comp z (next-lft))))))))))

(defun print-digits (n &optional (out *standard-output*))
  (let ((generator (pi-generator)))
    (labels ((print-digit (i)
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
