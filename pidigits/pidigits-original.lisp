;;; Unbounded Spigot algorithm for digits of pi
;;; http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf
;;;
;;; Implements the algorithm given on page 7
;;; Nearly a direct translation of the Haskell code from the paper to Lisp
;;; Uses streams code from SICP section 3.5 to emulate Haskell's lazy lists
;;;
;;; (PUSH :PIDIGITS-OPTIMIZED *FEATURES*) before compiling this file to use
;;; optimized versions of COMP and EXTR.
;;;
;;; On SBCL, (REQUIRE :SB-GMP) to use the gmplib for better bignum performance.
;;;
;;; To use, call (PRINT-DIGITS N) where N is the number of digits to print.

(defpackage #:pidigits-original
  (:use #:cl))

(in-package #:pidigits-original)

(declaim (optimize (speed 3) (debug 0) (space 0) (safety 0)))

;;; type utils

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

;;; streams from SICP

(declaim (ftype (function (function) function) memoize))
(defun memoize (fn)
  (let (runp result)
    (lambda ()
      (if (not runp)
          (setf runp t result (funcall fn))
          result))))

(defmacro delay (expr)
  `(memoize (lambda () ,expr)))

(declaim (ftype (function (function) t) force))
(defun force (x)
  (funcall x))

(defmacro cons-stream (x y)
  `(cons ,x (delay ,y)))

(defun stream-car (x)
  (car x))

(defun stream-cdr (x)
  (force (cdr x)))

(declaim (ftype (function (fixnum) cons) integers-from))
(defun integers-from (n)
  (cons-stream n (integers-from (1+ n))))

(declaim (ftype (function (function list) list) stream-map))
(defun stream-map (fn s)
  (when s
    (cons-stream (funcall fn (stream-car s))
                 (stream-map fn (stream-cdr s)))))

;;; spigot algorithm

(defstruct (lft
             (:constructor lft (zq zr zs zt))
             (:conc-name ""))
  (zq 0 :type integer)
  (zr 0 :type integer)
  (zs 0 :type integer)
  (zt 0 :type integer))

(declaim (ftype (function (lft lft) lft) comp))

#-pidigits-optimized
(defun comp (m1 m2)
  (declare (type lft m1 m2))
  (let ((zq (zq m1)) (zr (zr m1)) (zs (zs m1)) (zt (zt m1))
        (zu (zq m2)) (zv (zr m2)) (zw (zs m2)) (zx (zt m2)))
    (lft (+ (* zq zu) (* zr zw))
         (+ (* zq zv) (* zr zx))
         (+ (* zs zu) (* zt zw))
         (+ (* zs zv) (* zt zx)))))

#+pidigits-optimized
(defun comp (m1 m2)
  (let ((zq (zq m1)) (zr (zr m1)) (zt (zt m1))
        (zu (zq m2)) (zv (zr m2)) (zx (zt m2)))
    (lft (* zq zu)
         (+ (* zq zv) (* zr zx))
         0
         (* zt zx))))

(declaim (ftype (function (lft integer) (integer 0 9)) extr))

#-pidigits-optimized
(defun extr (m x)
  (declare (type lft m))
  (declare (type integer x))
  (floor (+ (* x (zq m)) (zr m))
         (+ (* x (zs m)) (zt m))))

#+pidigits-optimized
(defun extr (m x)
  (values (floor (+ (* x (zq m)) (zr m))
                 (zt m))))

(defun pi-stream ()
  (let ((init (lft 1 0 0 1))
        (lfts (stream-map (lambda (k)
                            (declare (type positive-fixnum k))
                            (lft k (+ (* 4 k) 2) 0 (+ (* 2 k) 1)))
                          (integers-from 1))))
    (labels ((next (z)
               (declare (type lft z))
               (the (integer 0 9) (extr z 3)))
             (safe (z n)
               (declare (type lft z))
               (declare (type (integer 0 9) n))
               (the boolean (= n (extr z 4))))
             (prod (z n)
               (declare (type lft z))
               (declare (type integer n))
               (the lft (comp (lft 10 (* -10 n) 0 1) z)))
             (stream (z list)
               (declare (type lft z))
               (declare (type list list))
               (the list (let ((y (next z)))
                           (if (safe z y)
                               (cons-stream y (stream (prod z y) list))
                               (stream (comp z (stream-car list)) (stream-cdr list)))))))
      (the list (stream init lfts)))))

(defun print-digits (n)
  (declare (type positive-fixnum n))
  (let ((stream (pi-stream)))
    (labels ((print-digit (i)
               (declare (type positive-fixnum i))
               (when (<= i n)
                 (princ (stream-car stream))
                 (cond
                   ((zerop (mod i 10))
                    (format t "~A:~D~%" #\Tab i))
                   ((= i n)
                    (loop :repeat (- 10 (mod i 10))
                       :do (princ #\Space))
                    (format t "~A:~D~%" #\Tab i)))
                 (setf stream (stream-cdr stream))
                 (print-digit (1+ i)))))
      (print-digit 1))))
