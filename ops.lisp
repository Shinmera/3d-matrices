#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

(define-ofun midentity (n)
  (case n
    (2 (mat2 '(1 0
               0 1)))
    (3 (mat3 '(1 0 0
               0 1 0
               0 0 1)))
    (4 (mat4 '(1 0 0 0
               0 1 0 0
               0 0 1 0
               0 0 0 1)))
    (T (let ((mat (matn n n)))
         (dotimes (i n mat)
           (setf (men mat i i) 1))))))

(declaim (ftype (function (mat mat-dim) vec) mcol))
(define-ofun mcol (mat n)
  (etypecase mat
    (mat2 (vec2 (me2 mat n 0)
                (me2 mat n 1)))
    (mat3 (vec3 (me3 mat n 0)
                (me3 mat n 1)
                (me3 mat n 2)))
    (mat4 (vec4 (me4 mat n 0)
                (me4 mat n 1)
                (me4 mat n 2)
                (me4 mat n 3)))))

(declaim (ftype (function (vec mat mat-dim) vec) (setf mcol)))
(define-ofun (setf mcol) (vec mat n)
  (etypecase mat
    (mat2
     (setf (me2 mat n 0) (vx vec))
     (setf (me2 mat n 1) (vy vec)))
    (mat3
     (setf (me3 mat n 0) (vx vec))
     (setf (me3 mat n 1) (vy vec))
     (setf (me3 mat n 2) (vz vec)))
    (mat4
     (setf (me4 mat n 0) (vx vec))
     (setf (me4 mat n 1) (vy vec))
     (setf (me4 mat n 2) (vz vec))
     (setf (me4 mat n 3) (vw vec))))
  vec)

(declaim (ftype (function (mat mat-dim) vec) mrow))
(defun mrow (mat n)
  (etypecase mat
    (mat2 (vec2 (me2 mat 0 n)
                (me2 mat 1 n)))
    (mat3 (vec3 (me3 mat 0 n)
                (me3 mat 1 n)
                (me3 mat 2 n)))
    (mat4 (vec4 (me4 mat 0 n)
                (me4 mat 1 n)
                (me4 mat 2 n)
                (me4 mat 3 n)))))

(declaim (ftype (function (vec mat mat-dim) vec) (setf mrow)))
(defun (setf mrow) (vec mat n)
  (etypecase mat
    (mat2
     (setf (me2 mat 0 n) (vx vec))
     (setf (me2 mat 1 n) (vy vec)))
    (mat3
     (setf (me3 mat 0 n) (vx vec))
     (setf (me3 mat 1 n) (vy vec))
     (setf (me3 mat 2 n) (vz vec)))
    (mat4
     (setf (me4 mat 0 n) (vx vec))
     (setf (me4 mat 1 n) (vy vec))
     (setf (me4 mat 2 n) (vz vec))
     (setf (me4 mat 3 n) (vw vec))))
  vec)

;; TODO
;; = /= < > <= >=
;; + - * /
;; determinant
;; inverses
;; transpose
;; adjoint
;; trace
;; upper-triangular
;; lower-triangular
;; diagonal
;; upgrade, downgrade
;; head, tail, left, right, segment, block, corners
;; LU, QR, eigenvalues
