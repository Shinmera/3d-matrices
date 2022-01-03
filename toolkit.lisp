#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

;; We choose this limit in order to ensure that matrix indices
;; always remain within fixnum range. I'm quite certain you don't
;; want to use matrices as big as this allows anyway. You'll want
;; BLAS/LAPACK and/or someone much smarter than me for that.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *matrix-limit* (min (floor (sqrt array-dimension-limit))
                              (floor (sqrt most-positive-fixnum)))))

(defvar *eps* (ecase *float-type*
                (single-float 0.00001f0)
                (double-float 0.00000000001d0)))
(declaim (type #.*float-type* *eps*))

(deftype mat-dim ()
  '(integer 0 #.(1- *matrix-limit*)))

(declaim (inline ensure-function))
(defun ensure-function (functionish)
  (etypecase functionish
    (function functionish)
    (symbol (fdefinition functionish))))

(declaim (ftype (function (float-type float-type) boolean) ~=))
(declaim (inline ~=))
(defun ~= (a b)
  (< (abs (- a b)) *eps*))

(declaim (ftype (function (float-type float-type) boolean) ~/=))
(declaim (inline ~/=))
(defun ~/= (a b)
  (<= *eps* (abs (- a b))))

(defmacro with-floats (&environment env bindings &body body)
  `(let ,(loop for (var val) in bindings
               collect `(,var (the ,*float-type* ,(ensure-float-param val env))))
     ,@body))

(defun intern* (&rest parts)
  (let ((*print-case* (readtable-case *readtable*))
        (*package* #.*package*))
    (intern (format NIL "~{~a~}" parts) #.*package*)))
