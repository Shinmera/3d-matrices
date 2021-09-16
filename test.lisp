#|
 This file is a part of 3d-vectors
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:3d-matrices-test
  (:nicknames #:org.shirakumo.flare.matrix.test)
  (:use #:cl #:parachute #:3d-matrices #:3d-vectors))
(in-package #:org.shirakumo.flare.matrix.test)

(defun ~= (a b)
  (< (abs (- a b)) 0.00001))

(define-test 3d-matrices)

(define-test struct
  :parent 3d-matrices)

(defmacro define-matrix-struct-test (n)
  (let* ((matx (3d-matrices::intern* 'mat n))
         (maty (3d-matrices::intern* 'mat (case n (2 3) (3 4) (4 2))))
         (mcopyx (3d-matrices::intern* 'mcopy n))
         (matx-p (3d-matrices::intern* matx '-p))
         (mirefx (3d-matrices::intern* 'miref n))
         (mcrefx (3d-matrices::intern* 'mcref n)))
    `(define-test ,matx
       :parent struct
       :compile-at :execute
       (of-type ,matx (,matx))
       (of-type ,matx (,mcopyx (,matx)))
       (fail (,mcopyx (,maty)))
       (true (,matx-p (,matx)))
       (false (,matx-p (,maty)))
       (is = 0 (,mirefx (,matx) 0))
       (is = 0 (,mcrefx (,matx) 0 0))
       (fail (,mirefx (,matx) -1))
       (fail (,mirefx (,matx) ,(* n n)))
       (fail (,mcrefx (,matx) ,n 0))
       (of-type ,matx (mat ,@(loop repeat (* n n) collect 0)))
       (of-type ,matx (mcopy (,matx)))
       (true (mat-p (,matx)))
       (is = 0 (miref (,matx) 0))
       (is = 0 (mcref (,matx) 0 0))
       (fail (miref (,matx) -1))
       (fail (miref (,matx) ,(* n n)))
       (fail (mcref (,matx) ,n 0))
       (is = ,n (mcols (,matx)))
       (is = ,n (mrows (,matx))))))

(define-matrix-struct-test 2)
(define-matrix-struct-test 3)
(define-matrix-struct-test 4)

(define-test matn
  :parent struct
  :compile-at :execute
  (of-type mat2 (matn 2 2))
  (of-type mat3 (matn 3 3))
  (of-type mat4 (matn 4 4))
  (of-type matn (matn 1 2))
  (of-type matn (mcopyn (matn 1 2)))
  (fail (mcopyn (mat2)))
  (true (matn-p (matn 1 2)))
  (false (matn-p (mat2)))
  (is = 0 (mirefn (matn 1 2) 0))
  (is = 0 (mcrefn (matn 1 2) 0 0))
  (fail (mirefn (matn 1 2) -1))
  (fail (mirefn (matn 1 2) 2))
  (fail (mcrefn (matn 1 2) 1 0))
  (fail (mat 1 2))
  (of-type matn (mcopy (matn 1 2)))
  (true (mat-p (matn 1 2)))
  (is = 0 (miref (matn 1 2) 0))
  (is = 0 (mcref (matn 1 2) 0 0))
  (fail (miref (matn 1 2) -1))
  (fail (miref (matn 1 2) 2))
  (fail (mcref (matn 1 2) 1 0))
  (is = 2 (mcols (matn 1 2)))
  (is = 1 (mrows (matn 1 2))))

(define-test comparison
  :parent 3d-matrices
  :depends-on (struct)
  (true (m= (mat2) 0))
  (true (m= 0 (mat2)))
  (false (m= (mat2) 1))
  (false (m= 1 (mat2)))
  (true (m= (mat2) (mat2)))
  (true (m= (matn 1 2) (matn 1 2)))
  (true (m= (mat 1 2 3 4) (mat 1 2 3 4)))
  (false (m= (mat 1 2 3 4) (mat 4 3 2 1)))
  (false (m= (mat 4 3 2 1) (mat 1 2 3 4)))
  (fail (m= (matn 1 2) (matn 2 1)))
  (fail (m= (mat2) (mat3)))
  (fail (m= (matn 2 3) (matn 3 2)))
  (true (m/= (mat2) 1))
  (false (m/= (mat2) 0))
  (true (m/= (mat 1 2 3 4) (mat 4 3 2 1)))
  (true (m/= (mat 1 1 1 1) (mat 1 1 1 0)))
  (false (m/= (mat2) (mat2)))
  (fail (m/= (mat2) (mat3)))
  (fail (m/= (matn 2 3) (matn 3 2)))
  (true (m< (mat2) 1))
  (false (m< 0 (mat2)))
  (true (m< (mat 1 2 3 4) (mat 2 3 4 5)))
  (false (m< (mat 1 2 3 4) (mat 1 2 3 4)))
  (false (m< (mat 0 0 0 0) (mat 0 1 1 1)))
  (fail (m< (mat2) (mat3)))
  (fail (m< (matn 2 3) (matn 3 2)))
  (true (m> (mat2) -1))
  (false (m> 0 (mat2)))
  (true (m> (mat 2 3 4 5) (mat 1 2 3 4)))
  (false (m> (mat 1 2 3 4) (mat 1 2 3 4)))
  (false (m> (mat 1 1 1 1) (mat 0 0 0 1)))
  (fail (m> (mat2) (mat3)))
  (fail (m> (matn 2 3) (matn 3 2)))
  (true (m<= (mat2) 0))
  (false (m<= 1 (mat2)))
  (true (m<= (mat 1 2 3 4) (mat 2 3 4 5)))
  (true (m<= (mat 1 2 3 4) (mat 1 2 3 4)))
  (true (m<= (mat 0 0 0 0) (mat 0 1 1 1)))
  (false (m<= (mat 1 1 1 1) (mat 0 0 0 0)))
  (fail (m<= (mat2) (mat3)))
  (fail (m<= (matn 2 3) (matn 3 2)))
  (true (m>= (mat2) 0))
  (false (m>= -1 (mat2)))
  (true (m>= (mat 2 3 4 5) (mat 1 2 3 4)))
  (true (m>= (mat 1 2 3 4) (mat 1 2 3 4)))
  (true (m>= (mat 1 1 1 1) (mat 0 0 0 1)))
  (false (m>= (mat 0 0 0 0) (mat 1 1 1 1)))
  (fail (m>= (mat2) (mat3)))
  (fail (m>= (matn 2 3) (matn 3 2))))

(define-test arithmetic
  :parent 3d-matrices
  :depends-on (comparison)
  (is m= (mat 1 1 1 1) (m+ 1 (mat2 0)))
  (is m= (mat 1 2 3 4) (m+ 1 (mat 0 1 2 3)))
  (is m= (mat 1 2 3 4) (m+ (mat 1 0 3 0) (mat 0 2 0 4)))
  (fail (m+ (mat2) (mat3)))
  (fail (m+ (matn 2 3) (matn 3 2)))
  (is m= (mat 0 0 0 0) (m- 1 (mat2 1)))
  (is m= (mat 0 1 2 3) (m- 1 (mat 1 2 3 4)))
  (is m= (mat 1 2 3 4) (m- (mat 2 2 4 4) (mat 1 0 1 0)))
  (fail (m- (mat2) (mat3)))
  (fail (m- (matn 2 3) (matn 3 2)))
  (is m= (mat 0 0 0 0) (m* 2 (mat 0 0 0 0)))
  (is m= (mat 2 4 6 8) (m* 2 (mat 1 2 3 4)))
  (is m= (mat 19 22 43 50) (m* (mat 1 2 3 4) (mat 5 6 7 8)))
  (is m= (mat 3 4 6 8) (m* (matn 2 1 '(1 2)) (matn 1 2 '(3 4))))
  (is v= (vec 17 39) (m* (mat 1 2 3 4) (vec 5 6)))
  (fail (m* (matn 2 1) (vec 0 0)))
  (fail (m* (matn 2 1) (mat4)))
  (fail (m* (matn 6 5) (matn 3 2)))
  (is m= (mat 1 1 1 1) (m/ 2 (mat 2 2 2 2)))
  (fail (m/ (mat2) (mat2)))
  (fail (m/ (matn 2 3) (matn 3 2)))
  (let ((mat (mat 1 2 3 4)))
    (is m= (mat 2 3 4 5) (nm+ mat 1))
    (is m= (mat 5 5 5 5) (nm+ mat (mat 3 2 1 0)))
    (is m= (mat 5 4 3 2) (nm- mat (mat 0 1 2 3)))
    (is m= (mat 4 3 2 1) (nm- mat 1))
    (is m= (mat 8 6 4 2) (nm* mat 2))
    (is m= (mat 4 3 2 1) (nm/ mat 2))
    (is m= (mat 13 20 5 8) (nm* mat (mat 1 2 3 4)))
    (is m= (mat 5 8 13 20) (n*m (mat 0 1 1 0) mat))))

(define-test construction
  :parent 3d-matrices
  :depends-on (comparison)
  (is m= (mat 1 0 0 1) (meye 2))
  (is equal '(1.0 1.0 1.0 1.0 1.0) (mdiag (meye 5)))
  (true (every (lambda (a) (<= 0.0 a 1.0)) (marr (mrand 20 20))))
  (true (every (lambda (a) (<= 1.0 a 2.0)) (marr (mrand 20 20 :min 1 :max 2))))
  (is m= (mat 1 1 1 1) (muniform 2 2 1))
  (is m= (matn 2 3 1)  (muniform 2 3 1)))

(define-test sectioning
  :parent 3d-matrices
  :depends-on (comparison)
  (is v= (vec 1 3) (mcol (mat 1 2 3 4) 0))
  (is v= (vec 1 4 7) (mcol (mat 1 2 3 4 5 6 7 8 9) 0))
  (is v= (vec 2 5 8) (mcol (mat 1 2 3 4 5 6 7 8 9) 1))
  (is v= (vec 1 2) (mrow (mat 1 2 3 4) 0))
  (is v= (vec 1 2 3) (mrow (mat 1 2 3 4 5 6 7 8 9) 0))
  (is v= (vec 4 5 6) (mrow (mat 1 2 3 4 5 6 7 8 9) 1))
  (is equal '(1.0 5.0 9.0) (mdiag (mat 1 2 3 4 5 6 7 8 9)))
  (is m= (mat 1 2 4 5) (mblock (mat 1 2 3 4 5 6 7 8 9) 0 0 2 2))
  (is m= (mat 5 6 8 9) (mblock (mat 1 2 3 4 5 6 7 8 9) 1 1 3 3))
  (is m= (matn 1 3 '(1 2 3)) (mtop (mat 1 2 3 4 5 6 7 8 9) 1))
  (is m= (matn 1 3 '(7 8 9)) (mbottom (mat 1 2 3 4 5 6 7 8 9) 1))
  (is m= (matn 3 1 '(1 4 7)) (mleft (mat 1 2 3 4 5 6 7 8 9) 1))
  (is m= (matn 3 1 '(3 6 9)) (mright (mat 1 2 3 4 5 6 7 8 9) 1))
  (let ((mat (mat 1 2 3 4 5 6 7 8 9)))
    (is m= (mat 7 8 9 4 5 6 1 2 3) (nmswap-row mat 0 2))
    (is m= (mat 8 7 9 5 4 6 2 1 3) (nmswap-col mat 0 1))))

(define-test matrix-math
  :parent 3d-matrices
  :depends-on (comparison)
  (is m= (mat 1 1 1 3 -1 -1 4 0 -1) (mlu (mat 1 1 1 3 2 2 4 4 3) NIL))
  (is m= (mat 4 4 3 0.75 -1 -0.25 0.25 0 0.25) (mlu (mat 1 1 1 3 2 2 4 4 3) T))
  (is ~= -2 (mdet (mat 1 2 3 4)))
  (is ~=  0 (mdet (mat 1 2 3 4 5 6 7 8 9)))
  (is ~=  0 (mdet (mat 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7)))
  (is ~= -1 (mdet (mat 1 1 1 1 3 2 2 2 4 4 3 3 5 5 5 4)))
  (is ~= 1 (mdet (mat 1 1 1 1 1 3 2 2 2 2 4 4 3 3 3 5 5 5 4 4 6 6 6 6 5)))
  (is m= (mat -2 1 1.5 -0.5) (minv (mat 1 2 3 4)))
  (fail (minv (mat 1 2 3 4 5 6 7 8 9)))
  (fail (minv (mat 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7)))
  (is m= (mat -2 1 0 0 -1 -1 1 0 -1 0 -1 1 5 0 0 -1) (minv (mat 1 1 1 1 3 2 2 2 4 4 3 3 5 5 5 4)))
  (is m= (meye 5) (mtranspose (meye 5)))
  (is m= (mat4 1) (mtranspose (mat4 1)))
  (is m= (mat 1 4 7 2 5 8 3 6 9) (mtranspose (mat 1 2 3 4 5 6 7 8 9)))
  (is m= (matn 3 2 '(1.0 4.0 2.0 5.0 3.0 6.0)) (mtranspose (matn 2 3 '(1 2 3 4 5 6))))
  (is m= (matn 3 4 '(1.0 4.0 7.0 10.0 2.0 5.0 8.0 11.0 3.0 6.0 9.0 12.0)) (mtranspose (matn 4 3 '(1 2 3 4 5 6 7 8 9 10 11 12))))
  (is ~= 10 (mtrace (mat 1 1 1 1 3 2 2 2 4 4 3 3 5 5 5 4)))
  (is ~= 15 (mtrace (mat 1 1 1 1 1 3 2 2 2 2 4 4 3 3 3 5 5 5 4 4 6 6 6 6 5)))
  (is = (mdet (mat 1 2 4 5)) (mminor (mat 1 2 3 4 5 6 7 8 9) 0 0))
  (is = (mdet (mat 1 3 7 9)) (mminor (mat 1 2 3 4 5 6 7 8 9) 1 1))
  (is = (mdet (mat 1 3 7 9)) (mcofactor (mat 1 2 3 4 5 6 7 8 9) 1 1))
  (is m= (mat -3 6 -3 6 -12 6 -3 6 -3) (mcof (mat 1 2 3 4 5 6 7 8 9)))
  (is m= (mat 2 -1 0 0 1 1 -1 0 1 0 1 -1 -5 0 0 1) (madj (mat 1 1 1 1 3 2 2 2 4 4 3 3 5 5 5 4)))
  (is m= (mat 2 1 1 1) (mpivot (mat 2 1 1 1)))
  (is m= (mat 1 0 0 1) (nth-value 1 (mpivot (mat 2 1 1 1))))
  (is =  0 (nth-value 2 (mpivot (mat 2 1 1 1))))
  (is m= (mat 2 1 1 1) (mpivot (mat 1 1 2 1)))
  (is m= (mat 0 1 1 0) (nth-value 1 (mpivot (mat 1 1 2 1))))
  (is =  1 (nth-value 2 (mpivot (mat 1 1 2 1))))
  (is m= (mat 6 1 1 1 1 5 1 1 1 1 3 1 1 1 1 2 1 1 1 1 1 1 1 1 1)
      (mpivot (mat 1 1 1 1 1 5 1 1 1 1 3 1 1 1 1 2 1 1 1 1 6 1 1 1 1)))
  (is ~= 18 (m1norm (mat 1 2 3 4 5 6 7 8 9)))
  (is ~= 24 (minorm (mat 1 2 3 4 5 6 7 8 9)))
  (is ~= (sqrt 285) (m2norm (mat 1 2 3 4 5 6 7 8 9)))
  (multiple-value-bind (Q R) (mqr (mat 1 1 1 3 2 2 4 4 3))
    (is m~= (mat 0.19611613  0.14269547  0.9701426
                 0.58834845 -0.80860764  7.450581e-9
                 0.78446454  0.5707819  -0.24253567) Q)
    (is m~= (mat 5.0990195 4.510671  3.7262068
                 0.0       0.8086077 0.23782583
                 0.0       0.0       0.24253565) R))
  (let ((values (meigen (mat 1 1 1 3 2 2 4 4 3) 50)))
    (is ~= 6.6264195 (first values))
    (is ~= -0.39977652 (second values))
    (is ~= -0.22664261 (third values))))

(define-test transforms
  :parent 3d-matrices
  :depends-on (comparison)
  (is m= (mat 1 0 0 5 0 1 0 6 0 0 1 7 0 0 0 1) (mtranslation (vec 5 6 7)))
  (is m= (mat 5 0 0 0 0 6 0 0 0 0 7 0 0 0 0 1) (mscaling (vec 5 6 7)))
  (let ((c (cos 90))
        (s (sin 90)))
    (is m= (mat 1 0 0 0 0 c (- s) 0 0 s c 0 0 0 0 1) (mrotation (vec 1 0 0) 90))
    (is m= (mat c 0 s 0 0 1 0 0 (- s) 0 c 0 0 0 0 1) (mrotation (vec 0 1 0) 90))
    (is m= (mat c (- s) 0 0 s c 0 0 0 0 1 0 0 0 0 1) (mrotation (vec 0 0 1) 90)))
  (let ((mat (mat 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)))
    (is m= (mat  0  1  2 11  4  5  6 39  8  9  0 27  2  3  4 25) (nmtranslate mat (vec 1 2 3)))
    (is m= (mat  0  1  0 11  8  5  0 39 16  9  0 27  4  3  0 25) (nmscale mat (vec 2 1 0)))
    (is m~= (mat 0.0 1.0 0.0 11.0 -3.584589 5.0 7.1519732 39.0 -7.169178 9.0 14.3039465 27.0
                 -1.7922945 3.0 3.5759866 25.0) (nmrotate mat (vec 0 1 0) 90))))
