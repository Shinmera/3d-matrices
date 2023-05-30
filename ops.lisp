#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

;;; PUBLIC APOLOGY FOR PEOPLE THAT ARE EITHER NOT ME, OR ARE FUTURE ME
;;;
;; Let me just state straight up that I am not at all happy with
;; a lot of the code in this file. There's lots of leaky macros
;; all over the place and often things are abbreviated and inlined
;; heavily for the sake of brevity and performance.
;;
;; If it helps at all, know at least that none of the leaky macros
;; will actually ever be exported or should ever have to be used
;; by anything outside of this very file.
;;
;; A lot of the explicit inlining could most definitely also be
;; gotten rid of by instead performing the algorithm generically at
;; compile time and spitting out the resulting forms. However, I'm
;; not bothering with that right now as I just want to get this over
;; with. If you want to improve the clarity of this code, you are
;; more than welcome to and I thank you heavily in advance. Really.
;; Thank you.

(defmacro with-fast-matref ((accessor mat width) &body body)
  (let ((w (gensym "WIDTH")) (arr (gensym "ARRAY"))
        (x (gensym "X")) (y (gensym "Y")))
    `(let ((,w ,width)
           (,arr (,(case width (2 'marr2) (3 'marr3) (4 'marr4) (T 'marr)) ,mat)))
       (declare (ignorable ,w))
       (macrolet ((,accessor (,y &optional ,x)
                    `(the ,*float-type*
                          (aref ,',arr ,(if ,x
                                            `(+ ,,x (* ,,y ,',(if (constantp width) width w)))
                                            ,y)))))
         ,@body))))

(defmacro with-fast-matrefs (bindings &body body)
  (if bindings
      `(with-fast-matref ,(first bindings)
         (with-fast-matrefs ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-fast-matcase ((accessor mat) &body body)
  (let ((mat2 (cdr (assoc 'mat2 body)))
        (mat3 (cdr (assoc 'mat3 body)))
        (mat4 (cdr (assoc 'mat4 body)))
        (matn (cdr (assoc 'matn body))))
    `(etypecase ,mat
       ,@(when mat2
           `((mat2 (with-fast-matref (,accessor ,mat 2)
                     ,@mat2))))
       ,@(when mat3
           `((mat3 (with-fast-matref (,accessor ,mat 3)
                     ,@mat3))))
       ,@(when mat4
           `((mat4 (with-fast-matref (,accessor ,mat 4)
                     ,@mat4))))
       ,@(when matn
           `((matn ,@matn))))))

(defmacro msetf (mat &rest els)
  (let ((m (gensym "MAT"))
        (arr (gensym "ARR")))
    `(let* ((,m ,mat)
            (,arr (marr ,m)))
       (psetf
        ,@(loop for el in els
                for i from 0
                collect `(aref ,arr ,i)
                collect `(ensure-float ,el)))
       ,m)))

(declaim (ftype (function (mat-dim) mat) meye))
(define-ofun meye (n)
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
         (do-mat-diag (i e mat mat)
           (setf e #.(ensure-float 1)))))))

(declaim (ftype (function (mat-dim mat-dim &key (:min real) (:max real)) mat) mrand))
(define-ofun mrand (r c &key (min 0) (max 1))
  (let ((mat (matn r c))
        (min (ensure-float min))
        (max (ensure-float max)))
    (map-into (marr mat) (lambda () (+ min (random (- max min)))))
    mat))

(declaim (ftype (function (mat-dim mat-dim real) mat) muniform))
(define-ofun muniform (r c element)
  (matn r c element))

(declaim (ftype (function (mat mat-dim) vec) mcol))
(define-ofun mcol (mat n)
  (with-fast-matcase (e mat)
    (mat2 (vec2 (e 0 n) (e 1 n)))
    (mat3 (vec3 (e 0 n) (e 1 n) (e 2 n)))
    (mat4 (vec4 (e 0 n) (e 1 n) (e 2 n) (e 3 n)))))

(declaim (ftype (function (vec mat mat-dim) vec) (setf mcol)))
(define-ofun (setf mcol) (vec mat n)
  (with-fast-matcase (e mat)
    (mat2 (setf (e 0 n) (vx vec) (e 1 n) (vy vec)))
    (mat3 (setf (e 0 n) (vx vec) (e 1 n) (vy vec) (e 2 n) (vz vec)))
    (mat4 (setf (e 0 n) (vx vec) (e 1 n) (vy vec) (e 2 n) (vz vec) (e 3 n) (vw vec))))
  vec)

(declaim (ftype (function (mat mat-dim) vec) mrow))
(define-ofun mrow (mat n)
  (with-fast-matcase (e mat)
    (mat2 (vec2 (e n 0) (e n 1)))
    (mat3 (vec3 (e n 0) (e n 1) (e n 2)))
    (mat4 (vec4 (e n 0) (e n 1) (e n 2) (e n 3)))))

(declaim (ftype (function (vec mat mat-dim) vec) (setf mrow)))
(define-ofun (setf mrow) (vec mat n)
  (with-fast-matcase (e mat)
    (mat2 (setf (e n 0) (vx vec) (e n 1) (vy vec)))
    (mat3 (setf (e n 0) (vx vec) (e n 1) (vy vec) (e n 2) (vz vec)))
    (mat4 (setf (e n 0) (vx vec) (e n 1) (vy vec) (e n 2) (vz vec) (e n 3) (vw vec))))
  vec)

(defmacro %2mat-op (a b c m2 m3 m4 mnmn mnr)
  (let ((m2 (if (listp m2) m2 (list m2)))
        (m3 (if (listp m3) m3 (list m3)))
        (m4 (if (listp m4) m4 (list m4)))
        (e (gensym "E")) (f (gensym "F")))
    (flet ((unroll (size &optional constant)
             (loop for i from 0 below size
                   collect `(,c (,e ,i) ,(if constant b `(,f ,i))))))
      `(progn
         (with-fast-matcase (,e ,a)
           (mat4 (etypecase ,b
                   (,*float-type* (,@m4 ,@(unroll 16 T)))
                   (mat4 (with-fast-matref (,f ,b 4) (,@m4 ,@(unroll 16))))))
           (mat3 (etypecase ,b
                   (,*float-type* (,@m4 ,@(unroll 9 T)))
                   (mat3 (with-fast-matref (,f ,b 3) (,@m3 ,@(unroll 9))))))
           (mat2 (etypecase ,b
                   (,*float-type* (,@m4 ,@(unroll 4 T)))
                   (mat2 (with-fast-matref (,f ,b 2) (,@m2 ,@(unroll 4))))))
           (matn (etypecase ,b
                   (,*float-type*
                    ,mnr)
                   (mat
                    (assert (and (= (mcols a) (mcols b))
                                 (= (mrows a) (mrows b))))
                    ,mnmn))))))))

(defmacro define-matcomp (name op &optional (comb 'and))
  (let ((2mat-name (intern* '2mat "-" name)))
    (flet ((unroll (size)
             (loop for i from 0 below size
                   collect `(,op a (e ,i)))))
      `(progn
         (declaim (ftype (function ((or mat real) (or mat real)) boolean) ,2mat-name))
         (declaim (ftype (function ((or mat real) &rest (or mat real)) boolean) ,name))
         (define-ofun ,2mat-name (a b)
           (cond ((realp a)
                  (let ((a (ensure-float a)))
                    (with-fast-matcase (e b)
                      (mat4 (,comb ,@(unroll 16)))
                      (mat3 (,comb ,@(unroll 9)))
                      (mat2 (,comb ,@(unroll 4)))
                      (matn (with-fast-matref (e b (%cols b))
                              (loop for i from 0 below (* (%rows b) (%cols b))
                                    ,(ecase comb (and 'always) (or 'thereis)) (,op a (e i))))))))
                 (T
                  (let ((b (if (realp b) (ensure-float b) b)))
                    (%2mat-op a b ,op ,comb ,comb ,comb
                              (with-fast-matrefs ((e a (%cols a))
                                                  (f b (%cols b)))
                                (unless (and (= (%cols a) (%cols b))
                                             (= (%rows a) (%rows b)))
                                  (error "Matrices are of incompatible size:~%~a~%~a"
                                         (write-matrix a NIL) (write-matrix b NIL)))
                                (loop for i from 0 below (* (%cols a) (%rows a))
                                      ,(ecase comb (and 'always) (or 'thereis)) (,op (e i) (f i))))
                              (with-fast-matref (e a (%cols a))
                                (loop for i from 0 below (* (%cols a) (%rows a))
                                      ,(ecase comb (and 'always) (or 'thereis)) (,op (e i) b))))))))
         (define-ofun ,name (val &rest vals)
           (loop for prev = val then next
                 for next in vals
                 always (,2mat-name prev next)))
         (define-compiler-macro ,name (val &rest vals)
           (case (length vals)
             (0 T)
             (1 `(,',2mat-name ,val ,(first vals)))
             (T `(and ,@(loop for prev = val then next
                              for next in vals
                              collect `(,',2mat-name ,prev ,next))))))))))

(define-matcomp m= =)
(define-matcomp m~= ~=)
(define-matcomp m/= /= or)
(define-matcomp m< <)
(define-matcomp m> >)
(define-matcomp m<= <=)
(define-matcomp m>= >=)

(defmacro define-matop (name nname op &optional body)
  (let ((2mat-name (intern* '2mat "-" name)))
    `(progn
       ,@(unless body
           `((declaim (ftype (function ((or mat real) &rest (or mat real)) mat) ,name))
             (declaim (ftype (function ((or mat real) (or mat real)) mat) ,2mat-name))))
       (define-ofun ,2mat-name (a b)
         (let ((a (if (realp a) b a))
               (b (if (realp a) a b)))
           (let ((b (if (realp b) (ensure-float b) b)))
             ,(if body
                  `(,body a b)
                  `(%2mat-op a b ,op mat mat mat
                             (let ((mat (matn (%rows a) (%cols a))))
                               (unless (and (= (%cols a) (%cols b))
                                            (= (%rows a) (%rows b)))
                                 (error "Matrices are of incompatible size:~%~a~%~a"
                                        (write-matrix a NIL) (write-matrix b NIL)))
                               (with-fast-matrefs ((e a (%cols a))
                                                   (f b (%cols b))
                                                   (g mat (%cols a)))
                                 (dotimes (i (* (%cols mat) (%rows mat)) mat)
                                   (setf (g i) (,op (e i) (f i))))))
                             (let ((mat (matn (%rows a) (%cols a))))
                               (with-fast-matrefs ((e a (%cols a))
                                                   (g mat (%cols a)))
                                 (dotimes (i (* (%cols mat) (%rows mat)) mat)
                                   (setf (g i) (,op (e i) b))))))))))
       (define-ofun ,name (val &rest vals)
         (cond ((cdr vals)
                (apply #',nname (,2mat-name val (first vals)) (rest vals)))
               (vals (,2mat-name val (first vals)))
               (T (mapply val #',op))))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(mapply ,val #',',op))
           (1 `(,',2mat-name ,val ,(first vals)))
           (T `(,',nname (,',2mat-name ,val ,(first vals)) ,@(rest vals))))))))

(defmacro define-nmatop (name op &optional body)
  (let ((2mat-name (intern* '2mat "-" name)))
    `(progn
       ,@(unless body
           `((declaim (ftype (function ((or mat real) &rest (or mat real)) mat) ,name))
             (declaim (ftype (function ((or mat real) (or mat real)) mat) ,2mat-name))))
       (define-ofun ,2mat-name (a b)
         (let ((b (if (realp b) (ensure-float b) b)))
           ,(if body
                `(,body a b)
                `(%2mat-op a b ,op (matf a) (matf a) (matf a)
                           (with-fast-matrefs ((e a (%cols a))
                                               (f b (%cols b)))
                             (unless (and (= (%cols a) (%cols b))
                                          (= (%rows a) (%rows b)))
                               (error "Matrices are of incompatible size:~%~a~%~a"
                                      (write-matrix a NIL) (write-matrix b NIL)))
                             (dotimes (i (* (%cols a) (%rows a)) a)
                               (setf (e i) (,op (e i) (f i)))))
                           (with-fast-matref (e a (%cols a))
                             (dotimes (i (* (%cols a) (%rows a)) a)
                               (setf (e i) (,op (e i) b))))))))
       (define-ofun ,name (val &rest vals)
         (if vals
             (loop for v in vals
                   do (setf val (,2mat-name val v))
                   finally (return val))
             (mapplyf val #',op)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(mapplyf ,val #',',op))
           (1 `(,',2mat-name ,val ,(first vals)))
           (T `(,',name (,',2mat-name ,val ,(first vals)) ,@(rest vals))))))))

(define-matop m+ nm+ +)
(define-nmatop nm+ +)
(define-matop m- nm- -)
(define-nmatop nm- -)

(defmacro %2mat*-expansion (a b)
  (let ((m (gensym "M"))
        (ncols (gensym "NCOLS")))
    (flet ((unroll (size) (loop for i from 0 below size collect `(* (e ,i) ,b)))
           (unroll-axb (rows-a cols-a cols-b) (loop for i from 0 below rows-a
                                         append (loop for j from 0 below cols-b
                                                      collect `(+ ,@(loop for k from 0 below cols-a
                                                                          collect `(* (e ,i ,k) (f ,k ,j))))))))
      `(let ((,a (if (vec-p ,a) ,b ,a))
             (,b (if (vec-p ,a) ,a ,b)))
         (flet ((matmul (a b)
                  (let ((m (matn (mrows a) (mcols b))))
                      (with-fast-matrefs ((e a (mcols a))
                                          (f b (mcols b))
                                          (g m (mcols b)))
                        (dotimes (i (mrows a) m)
                          (loop for sum of-type #.*float-type* = ,(ensure-float 0)
                                for j from 0 below (mcols b)
                                do (loop for k from 0 below (mcols a)
                                         do (setf sum (+ (* (e i k) (f k j)) sum)))
                                   (setf (g i j) sum))))
                    m)))
           (with-fast-matcase (e a)
             (mat2 (etypecase ,b
                     (,*float-type* (mat ,@(unroll 4)))
                     (vec2 (vec2 (+ (* (vx2 ,b) (e 0 0)) (* (vy2 ,b) (e 0 1)))
                                 (+ (* (vx2 ,b) (e 1 0)) (* (vy2 ,b) (e 1 1)))))
                     (mat2 (with-fast-matref (f b 2) (mat ,@(unroll-axb 2 2 2))))
                     (matn
                      (unless (= (mcols ,a) (mrows ,b))
                        (error "Matrices are of incompatible size:~%~a~%~a"
                               (write-matrix a NIL) (write-matrix b NIL)))
                      (let ((,ncols (mcols b)))
                        (if (< 5 ,ncols)
                            (matmul ,a ,b)
                            (ecase ,ncols
                              (1 (with-fast-matref (f b ,ncols) (matn 2 ,ncols (list ,@(unroll-axb 2 2 1)))))
                              (3 (with-fast-matref (f b ,ncols) (matn 2 ,ncols (list ,@(unroll-axb 2 2 3)))))
                              (4 (with-fast-matref (f b ,ncols) (matn 2 ,ncols (list ,@(unroll-axb 2 2 4)))))
                              (5 (with-fast-matref (f b ,ncols) (matn 2 ,ncols (list ,@(unroll-axb 2 2 5)))))))))))
             (mat3 (etypecase ,b
                     (,*float-type* (mat ,@(unroll 9)))
                     (vec3 (vec3 (+ (* (vx3 ,b) (e 0 0)) (* (vy3 ,b) (e 0 1)) (* (vz3 ,b) (e 0 2)))
                                 (+ (* (vx3 ,b) (e 1 0)) (* (vy3 ,b) (e 1 1)) (* (vz3 ,b) (e 1 2)))
                                 (+ (* (vx3 ,b) (e 2 0)) (* (vy3 ,b) (e 2 1)) (* (vz3 ,b) (e 2 2)))))
                     (mat3 (with-fast-matref (f b 3) (mat ,@(unroll-axb 3 3 3))))
                     (matn
                      (unless (= (mcols ,a) (mrows ,b))
                        (error "Matrices are of incompatible size:~%~a~%~a"
                               (write-matrix a NIL) (write-matrix b NIL)))
                      (let ((,ncols (mcols b)))
                        (if (< 5 ,ncols)
                            (matmul ,a ,b)
                         (ecase ,ncols
                           (1 (with-fast-matref (f b ,ncols) (matn 3 ,ncols (list ,@(unroll-axb 3 3 1)))))
                           (2 (with-fast-matref (f b ,ncols) (matn 3 ,ncols (list ,@(unroll-axb 3 3 2)))))
                           (4 (with-fast-matref (f b ,ncols) (matn 3 ,ncols (list ,@(unroll-axb 3 3 4)))))
                           (5 (with-fast-matref (f b ,ncols) (matn 3 ,ncols (list ,@(unroll-axb 3 3 5)))))))))))
             (mat4 (etypecase ,b
                     (,*float-type* (mat ,@(unroll 16)))
                     (vec3 (vec3 (+ (* (vx3 ,b) (e 0 0)) (* (vy3 ,b) (e 0 1)) (* (vz3 ,b) (e 0 2)) (*          (e 0 3)))
                                 (+ (* (vx3 ,b) (e 1 0)) (* (vy3 ,b) (e 1 1)) (* (vz3 ,b) (e 1 2)) (*          (e 1 3)))
                                 (+ (* (vx3 ,b) (e 2 0)) (* (vy3 ,b) (e 2 1)) (* (vz3 ,b) (e 2 2)) (*          (e 2 3)))))
                     (vec4 (vec4 (+ (* (vx4 ,b) (e 0 0)) (* (vy4 ,b) (e 0 1)) (* (vz4 ,b) (e 0 2)) (* (vw4 ,b) (e 0 3)))
                                 (+ (* (vx4 ,b) (e 1 0)) (* (vy4 ,b) (e 1 1)) (* (vz4 ,b) (e 1 2)) (* (vw4 ,b) (e 1 3)))
                                 (+ (* (vx4 ,b) (e 2 0)) (* (vy4 ,b) (e 2 1)) (* (vz4 ,b) (e 2 2)) (* (vw4 ,b) (e 2 3)))
                                 (+ (* (vx4 ,b) (e 3 0)) (* (vy4 ,b) (e 3 1)) (* (vz4 ,b) (e 3 2)) (* (vw4 ,b) (e 3 3)))))
                     (mat4 (with-fast-matref (f b 4) (mat ,@(unroll-axb 4 4 4))))
                     (matn
                      (unless (= (mcols ,a) (mrows ,b))
                        (error "Matrices are of incompatible size:~%~a~%~a"
                               (write-matrix a NIL) (write-matrix b NIL)))
                      (let ((,ncols (mcols b)))
                        (if (< 5 ,ncols)
                            (matmul ,a ,b)
                         (ecase ,ncols
                           (1 (with-fast-matref (f b ,ncols) (matn 4 ,ncols (list ,@(unroll-axb 4 4 1)))))
                           (2 (with-fast-matref (f b ,ncols) (matn 4 ,ncols (list ,@(unroll-axb 4 4 2)))))
                           (3 (with-fast-matref (f b ,ncols) (matn 4 ,ncols (list ,@(unroll-axb 4 4 3)))))
                           (5 (with-fast-matref (f b ,ncols) (matn 4 ,ncols (list ,@(unroll-axb 4 4 5)))))))))))
             (matn (etypecase ,b
                     (,*float-type*
                      (let ((,m (matn (%rows ,a) (%cols ,a))))
                        (map-into (marrn ,m) (lambda (,m) (* (the ,*float-type* ,m)
                                                             (the ,*float-type* ,b))) (marrn ,a))
                        ,m))
                     ((or mat2 mat3 mat4 matn)
                      (unless (= (mcols ,a) (mrows ,b))
                        (error "Matrices are of incompatible size:~%~a~%~a"
                               (write-matrix a NIL) (write-matrix b NIL)))
                      (matmul ,a ,b))))))))))

(defmacro %2nmat*-expansion (a b &optional (u a))
  (let ((m (gensym "M")))
    (flet ((unroll (size) (loop for i from 0 below size collect `(* (e ,i) ,b)))
           (unrollm (size) (loop for i from 0 below size
                                 append (loop for j from 0 below size
                                              collect `(+ ,@(loop for k from 0 below size
                                                                  collect `(* (e ,i ,k) (f ,k ,j))))))))
      `(let ((,a (if (or (realp ,a) (vec-p ,a)) ,b ,a))
             (,b (if (or (realp ,a) (vec-p ,a)) ,a ,b)))
         (with-fast-matcase (e a)
           (mat2 (etypecase ,b
                   (,*float-type* (matf ,a ,@(unroll 4)))
                   (vec2 (3d-vectors::%vsetf ,b (+ (* (vx2 ,b) (e 0 0)) (* (vy2 ,b) (e 0 1)))
                                                (+ (* (vx2 ,b) (e 1 0)) (* (vy2 ,b) (e 1 1)))))
                   (mat2 (with-fast-matref (f ,b 2) (matf ,u ,@(unrollm 2))))))
           (mat3 (etypecase ,b
                      (,*float-type* (matf ,a ,@(unroll 9)))
                      (vec3 (3d-vectors::%vsetf ,b
                                                (+ (* (vx3 ,b) (e 0 0)) (* (vy3 ,b) (e 0 1)) (* (vz3 ,b) (e 0 2)))
                                                (+ (* (vx3 ,b) (e 1 0)) (* (vy3 ,b) (e 1 1)) (* (vz3 ,b) (e 1 2)))
                                                (+ (* (vx3 ,b) (e 2 0)) (* (vy3 ,b) (e 2 1)) (* (vz3 ,b) (e 2 2)))))
                      (mat3 (with-fast-matref (f ,b 3) (matf ,u ,@(unrollm 3))))))
           (mat4 (etypecase ,b
                   (,*float-type* (matf ,a ,@(unroll 16)))
                   (vec3 (3d-vectors::%vsetf ,b
                                             (+ (* (vx3 ,b) (e 0 0)) (* (vy3 ,b) (e 0 1)) (* (vz3 ,b) (e 0 2)) (*          (e 0 3)))
                                             (+ (* (vx3 ,b) (e 1 0)) (* (vy3 ,b) (e 1 1)) (* (vz3 ,b) (e 1 2)) (*          (e 1 3)))
                                             (+ (* (vx3 ,b) (e 2 0)) (* (vy3 ,b) (e 2 1)) (* (vz3 ,b) (e 2 2)) (*          (e 2 3)))))
                   (vec4 (3d-vectors::%vsetf ,b
                                             (+ (* (vx4 ,b) (e 0 0)) (* (vy4 ,b) (e 0 1)) (* (vz4 ,b) (e 0 2)) (* (vw4 ,b) (e 0 3)))
                                             (+ (* (vx4 ,b) (e 1 0)) (* (vy4 ,b) (e 1 1)) (* (vz4 ,b) (e 1 2)) (* (vw4 ,b) (e 1 3)))
                                             (+ (* (vx4 ,b) (e 2 0)) (* (vy4 ,b) (e 2 1)) (* (vz4 ,b) (e 2 2)) (* (vw4 ,b) (e 2 3)))
                                             (+ (* (vx4 ,b) (e 3 0)) (* (vy4 ,b) (e 3 1)) (* (vz4 ,b) (e 3 2)) (* (vw4 ,b) (e 3 3)))))
                   (mat4 (with-fast-matref (f ,b 4) (matf ,u ,@(unrollm 4))))))
           (matn (etypecase ,b
                   (,*float-type*
                    (map-into (marrn ,a) (lambda (,m) (* (the ,*float-type* ,m)
                                                          (the ,*float-type* ,b))) (marrn ,a)))
                   (matn
                    (unless (= (%cols a) (%cols b) (%rows a) (%rows b))
                      (error "Matrices are of incompatible size (only square matrices can be multiplied in place!):~%~a~%~a"
                             (write-matrix a NIL) (write-matrix b NIL)))
                    (let ((,m (make-array (%cols ,a) :initial-element ,(ensure-float 0) :element-type ',*float-type*))
                          (s (%rows ,a)))
                      (with-fast-matrefs ((e ,a s)
                                          (f ,b s)
                                          (g ,u s))
                        (dotimes (,(if (eq u a) 'i 'j) s)
                          (loop for sum of-type ,*float-type* = ,(ensure-float 0)
                                for ,(if (eq u a) 'j 'i) from 0 below s
                                do (loop for k from 0 below s
                                         do (setf sum (+ (* (e i k) (f k j)) sum)))
                                   (setf (aref ,m ,(if (eq u a) 'j 'i)) sum))
                          (loop for ,(if (eq u a) 'j 'i) from 0 below s
                                do (setf (g i j) (aref ,m ,(if (eq u a) 'j 'i)))))))))
                 ,u))))))

(defmacro %2n*mat-expansion (a b)
  `(%2nmat*-expansion ,a ,b ,b))

(declaim (ftype (function ((or mat vec real) &rest (or vec mat real)) (or mat vec)) m* nm*))
(declaim (ftype (function ((or mat vec real) (or vec mat real)) (or mat vec)) 2mat-m* 2mat-nm*))
;; We can't always use the modifying variant for m* as the sizes can change depending on how you multiply!
(define-matop m* m* * %2mat*-expansion)
;; For the modifying variant the user will just have to watch out to ensure square sizes.
(define-nmatop nm* * %2nmat*-expansion)
(define-nmatop n*m * %2n*mat-expansion)

;; We only allow element-wise division.
(defmacro %2mat/-expansion (a b)
  `(%2nmat/-expansion ,a ,b (mat) (matn (%rows ,a) (%cols ,a))))

(defmacro %2nmat/-expansion (a b &optional (mc `(matf ,a)) (mn a))
  (let ((m (gensym "M")))
    (flet ((unroll (size) (loop for i from 0 below size collect `(/ (e ,i) ,b))))
      `(with-fast-matcase (e ,a)
         (mat2 (etypecase ,b (,*float-type* (,@mc ,@(unroll 4)))))
         (mat3 (etypecase ,b (,*float-type* (,@mc ,@(unroll 9)))))
         (mat4 (etypecase ,b (,*float-type* (,@mc ,@(unroll 16)))))
         (matn (etypecase ,b (,*float-type* (let ((,m ,mn))
                                              (map-into (marrn ,m) (lambda (,m) (* (the ,*float-type* ,m) ,b)) (marrn ,a))
                                              ,mn))))))))

(declaim (ftype (function ((or mat real) &rest (or mat real)) mat) m/ nm/))
(declaim (ftype (function ((or mat real) (or mat real)) mat) 2mat-m/ 2mat-nm/))
(define-matop m/ nm/ / %2mat/-expansion)
(define-nmatop nm/ / %2nmat/-expansion)

(declaim (inline mapply))
(declaim (ftype (function (mat (or symbol function)) mat) mapply))
(define-ofun mapply (mat op)
  (etypecase mat
    (mat2 (let ((m (mat2))) (map-into (marr2 m) op (marr2 mat)) m))
    (mat3 (let ((m (mat3))) (map-into (marr3 m) op (marr3 mat)) m))
    (mat4 (let ((m (mat4))) (map-into (marr4 m) op (marr4 mat)) m))
    (matn (let ((m (matn (%rows mat) (%cols mat))))
            (map-into (marrn m) op (marrn mat))
            m))))

(declaim (inline mapplyf))
(declaim (ftype (function (mat (or symbol function)) mat) mapplyf))
(define-ofun mapplyf (mat op)
  (etypecase mat
    (mat2 (map-into (marr2 mat) op (marr2 mat)) mat)
    (mat3 (map-into (marr3 mat) op (marr3 mat)) mat)
    (mat4 (map-into (marr4 mat) op (marr4 mat)) mat)
    (matn (map-into (marrn mat) op (marrn mat)) mat)))

;; Lots of juicy inlined crap.
(declaim (ftype (function (mat) float-type) mdet))
(define-ofun mdet (m)
  (with-fast-matcase (e m)
    (mat2 (- (* (e 0 0) (e 1 1))
             (* (e 0 1) (e 1 0))))
    (mat3 (- (+ (* (e 0 0) (e 1 1) (e 2 2))
                (* (e 0 1) (e 1 2) (e 2 0))
                (* (e 0 2) (e 1 0) (e 2 1)))
             (+ (* (e 0 0) (e 1 2) (e 2 1))
                (* (e 0 1) (e 1 0) (e 2 2))
                (* (e 0 2) (e 1 1) (e 2 0)))))
    (mat4 (- (+ (* (e 0 3) (e 1 2) (e 2 1) (e 3 0)) (* (e 0 0) (e 1 1) (e 2 2) (e 3 3))
                (* (e 0 1) (e 1 3) (e 2 2) (e 3 0)) (* (e 0 2) (e 1 1) (e 2 3) (e 3 0))
                (* (e 0 2) (e 1 3) (e 2 0) (e 3 1)) (* (e 0 3) (e 1 0) (e 2 2) (e 3 1))
                (* (e 0 0) (e 1 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 2 0) (e 3 2))
                (* (e 0 0) (e 1 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 1 0) (e 2 3) (e 3 2))
                (* (e 0 1) (e 1 2) (e 2 0) (e 3 3)) (* (e 0 2) (e 1 0) (e 2 1) (e 3 3)))
             (+ (* (e 0 2) (e 1 3) (e 2 1) (e 3 0)) (* (e 0 3) (e 1 1) (e 2 2) (e 3 0))
                (* (e 0 1) (e 1 2) (e 2 3) (e 3 0)) (* (e 0 3) (e 1 2) (e 2 0) (e 3 1))
                (* (e 0 0) (e 1 3) (e 2 2) (e 3 1)) (* (e 0 2) (e 1 0) (e 2 3) (e 3 1))
                (* (e 0 1) (e 1 3) (e 2 0) (e 3 2)) (* (e 0 3) (e 1 0) (e 2 1) (e 3 2))
                (* (e 0 0) (e 1 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 2 0) (e 3 3))
                (* (e 0 0) (e 1 2) (e 2 1) (e 3 3)) (* (e 0 1) (e 1 0) (e 2 2) (e 3 3)))))
    (matn (multiple-value-bind (LU P s) (mlu m)
            (declare (ignore P))
            (with-fast-matref (lu LU (%rows LU))
              (loop for det = #.(ensure-float 1) then (* (expt -1.0 (the integer s))
                                                         (the float-type det)
                                                         (lu i i))
                    for i from 0 below (%rows LU)
                    finally (return det)))))))

;; More of the same.
(declaim (ftype (function (mat) mat) minv))
(define-ofun minv (m)
  (let ((det (/ (mdet m))))
    (with-fast-matcase (e m)
      (mat2 (mat (* det (+ (e 1 1)))
                 (* det (- (e 0 1)))
                 (* det (- (e 1 0)))
                 (* det (+ (e 0 0)))))
      (mat3 (mat (* det (- (* (e 1 1) (e 2 2)) (* (e 1 2) (e 2 1))))
                 (* det (- (* (e 0 2) (e 2 1)) (* (e 0 1) (e 2 2))))
                 (* det (- (* (e 0 1) (e 1 2)) (* (e 0 2) (e 1 1))))
                 (* det (- (* (e 1 2) (e 2 0)) (* (e 1 0) (e 2 2))))
                 (* det (- (* (e 0 0) (e 2 2)) (* (e 0 2) (e 2 0))))
                 (* det (- (* (e 0 2) (e 1 0)) (* (e 0 0) (e 1 2))))
                 (* det (- (* (e 1 0) (e 2 1)) (* (e 1 1) (e 2 0))))
                 (* det (- (* (e 0 1) (e 2 0)) (* (e 0 0) (e 2 1))))
                 (* det (- (* (e 0 0) (e 1 1)) (* (e 0 1) (e 1 0))))))
      (mat4 (mat (* det (- (+ (* (e 1 2) (e 2 3) (e 3 1)) (* (e 1 3) (e 2 1) (e 3 2)) (* (e 1 1) (e 2 2) (e 3 3)))
                           (+ (* (e 1 3) (e 2 2) (e 3 1)) (* (e 1 1) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 1) (e 3 3)))))
                 (* det (- (+ (* (e 0 3) (e 2 2) (e 3 1)) (* (e 0 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 1) (e 3 3)))
                           (+ (* (e 0 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 2 2) (e 3 3)))))
                 (* det (- (+ (* (e 0 2) (e 1 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 3 2)) (* (e 0 1) (e 1 2) (e 3 3)))
                           (+ (* (e 0 3) (e 1 2) (e 3 1)) (* (e 0 1) (e 1 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 3 3)))))
                 (* det (- (+ (* (e 0 3) (e 1 2) (e 2 1)) (* (e 0 1) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 1) (e 2 3)))
                           (+ (* (e 0 2) (e 1 3) (e 2 1)) (* (e 0 3) (e 1 1) (e 2 2)) (* (e 0 1) (e 1 2) (e 2 3)))))
                 (* det (- (+ (* (e 1 3) (e 2 2) (e 3 0)) (* (e 1 0) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 0) (e 3 3)))
                           (+ (* (e 1 2) (e 2 3) (e 3 0)) (* (e 1 3) (e 2 0) (e 3 2)) (* (e 1 0) (e 2 2) (e 3 3)))))
                 (* det (- (+ (* (e 0 2) (e 2 3) (e 3 0)) (* (e 0 3) (e 2 0) (e 3 2)) (* (e 0 0) (e 2 2) (e 3 3)))
                           (+ (* (e 0 3) (e 2 2) (e 3 0)) (* (e 0 0) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 0) (e 3 3)))))
                 (* det (- (+ (* (e 0 3) (e 1 2) (e 3 0)) (* (e 0 0) (e 1 3) (e 3 2)) (* (e 0 2) (e 1 0) (e 3 3)))
                           (+ (* (e 0 2) (e 1 3) (e 3 0)) (* (e 0 3) (e 1 0) (e 3 2)) (* (e 0 0) (e 1 2) (e 3 3)))))
                 (* det (- (+ (* (e 0 2) (e 1 3) (e 2 0)) (* (e 0 3) (e 1 0) (e 2 2)) (* (e 0 0) (e 1 2) (e 2 3)))
                           (+ (* (e 0 3) (e 1 2) (e 2 0)) (* (e 0 0) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 0) (e 2 3)))))
                 (* det (- (+ (* (e 1 1) (e 2 3) (e 3 0)) (* (e 1 3) (e 2 0) (e 3 1)) (* (e 1 0) (e 2 1) (e 3 3)))
                           (+ (* (e 1 3) (e 2 1) (e 3 0)) (* (e 1 0) (e 2 3) (e 3 1)) (* (e 1 1) (e 2 0) (e 3 3)))))
                 (* det (- (+ (* (e 0 3) (e 2 1) (e 3 0)) (* (e 0 0) (e 2 3) (e 3 1)) (* (e 0 1) (e 2 0) (e 3 3)))
                           (+ (* (e 0 1) (e 2 3) (e 3 0)) (* (e 0 3) (e 2 0) (e 3 1)) (* (e 0 0) (e 2 1) (e 3 3)))))
                 (* det (- (+ (* (e 0 1) (e 1 3) (e 3 0)) (* (e 0 3) (e 1 0) (e 3 1)) (* (e 0 0) (e 1 1) (e 3 3)))
                           (+ (* (e 0 3) (e 1 1) (e 3 0)) (* (e 0 0) (e 1 3) (e 3 1)) (* (e 0 1) (e 1 0) (e 3 3)))))
                 (* det (- (+ (* (e 0 3) (e 1 1) (e 2 0)) (* (e 0 0) (e 1 3) (e 2 1)) (* (e 0 1) (e 1 0) (e 2 3)))
                           (+ (* (e 0 1) (e 1 3) (e 2 0)) (* (e 0 3) (e 1 0) (e 2 1)) (* (e 0 0) (e 1 1) (e 2 3)))))
                 (* det (- (+ (* (e 1 2) (e 2 1) (e 3 0)) (* (e 1 0) (e 2 2) (e 3 1)) (* (e 1 1) (e 2 0) (e 3 2)))
                           (+ (* (e 1 1) (e 2 2) (e 3 0)) (* (e 1 2) (e 2 0) (e 3 1)) (* (e 1 0) (e 2 1) (e 3 2)))))
                 (* det (- (+ (* (e 0 1) (e 2 2) (e 3 0)) (* (e 0 2) (e 2 0) (e 3 1)) (* (e 0 0) (e 2 1) (e 3 2)))
                           (+ (* (e 0 2) (e 2 1) (e 3 0)) (* (e 0 0) (e 2 2) (e 3 1)) (* (e 0 1) (e 2 0) (e 3 2)))))
                 (* det (- (+ (* (e 0 2) (e 1 1) (e 3 0)) (* (e 0 0) (e 1 2) (e 3 1)) (* (e 0 1) (e 1 0) (e 3 2)))
                           (+ (* (e 0 1) (e 1 2) (e 3 0)) (* (e 0 2) (e 1 0) (e 3 1)) (* (e 0 0) (e 1 1) (e 3 2)))))
                 (* det (- (+ (* (e 0 1) (e 1 2) (e 2 0)) (* (e 0 2) (e 1 0) (e 2 1)) (* (e 0 0) (e 1 1) (e 2 2)))
                           (+ (* (e 0 2) (e 1 1) (e 2 0)) (* (e 0 0) (e 1 2) (e 2 1)) (* (e 0 1) (e 1 0) (e 2 2)))))))
      (matn (nm* (madj m) det)))))

(declaim (ftype (function (mat) mat) mtranspose))
(define-ofun mtranspose (m)
  (with-fast-matcase (e m)
    (mat2 (mat (e 0 0) (e 1 0)
               (e 0 1) (e 1 1)))
    (mat3 (mat (e 0 0) (e 1 0) (e 2 0)
               (e 0 1) (e 1 1) (e 2 1)
               (e 0 2) (e 1 2) (e 2 2)))
    (mat4 (mat (e 0 0) (e 1 0) (e 2 0) (e 3 0)
               (e 0 1) (e 1 1) (e 2 1) (e 3 1)
               (e 0 2) (e 1 2) (e 2 2) (e 3 2)
               (e 0 3) (e 1 3) (e 2 3) (e 3 3)))
    (matn (let ((r (matn (%cols m) (%rows m))))
            (with-fast-matrefs ((r r (%cols r))
                                (m m (%cols m)))
              (dotimes (y (%rows r) r)
                (dotimes (x (%cols r))
                  (setf (r y x) (m x y)))))))))

(declaim (ftype (function (mat) mat) mtranspose))
(define-ofun nmtranspose (m)
  (with-fast-matcase (e m)
    (mat2 (rotatef (e 0 1) (e 1 0)))
    (mat3 (rotatef (e 0 1) (e 1 0))
          (rotatef (e 0 2) (e 2 0))
          (rotatef (e 1 2) (e 2 1)))
    (mat4 (rotatef (e 0 1) (e 1 0))
          (rotatef (e 0 2) (e 2 0))
          (rotatef (e 0 3) (e 3 0))
          (rotatef (e 1 2) (e 2 1))
          (rotatef (e 1 3) (e 3 1))
          (rotatef (e 2 3) (e 3 2))))
  m)

(declaim (ftype (function (mat) float-type) mtrace))
(define-ofun mtrace (m)
  (with-fast-matcase (e m)
    (mat2 (+ (e 0 0) (e 1 1)))
    (mat3 (+ (e 0 0) (e 1 1) (e 2 2)))
    (mat4 (+ (e 0 0) (e 1 1) (e 2 2) (e 3 3)))
    (matn (let ((sum #.(ensure-float 0)))
            (do-mat-diag (i el m sum)
              (setf sum (+ el (the float-type sum))))))))

(declaim (ftype (function (mat mat-dim mat-dim) float-type) mminor))
(define-ofun mminor (m y x)
  (let* ((c (mcols m))
         (r (mrows m))
         (s (matn (1- r) (1- c))))
    (with-fast-matrefs ((s s (1- c))
                        (m m c))
      ;; Copy the four subregions
      (loop for i from 0 below y
            do (loop for j from 0 below x
                     do (setf (s i j) (m i j)))
               (loop for j from (1+ x) below c
                     do (setf (s i (1- j)) (m i j))))
      (loop for i from (1+ y) below r
            do (loop for j from 0 below x
                     do (setf (s (1- i) j) (m i j)))
               (loop for j from (1+ x) below c
                     do (setf (s (1- i) (1- j)) (m i j))))
      (mdet s))))

(declaim (ftype (function (mat mat-dim mat-dim) float-type) mcoefficient))
(define-ofun mcofactor (m y x)
  (* (if (evenp (+ y x)) 1 -1) (mminor m y x)))

(declaim (ftype (function (mat) mat) mcof))
(define-ofun mcof (m)
  (etypecase m
    (mat2 (mat (mcofactor m 0 0) (mcofactor m 0 1)
               (mcofactor m 1 0) (mcofactor m 1 1)))
    (mat3 (mat (mcofactor m 0 0) (mcofactor m 0 1) (mcofactor m 0 2)
               (mcofactor m 1 0) (mcofactor m 1 1) (mcofactor m 1 2)
               (mcofactor m 2 0) (mcofactor m 2 1) (mcofactor m 2 2)))
    (mat4 (mat (mcofactor m 0 0) (mcofactor m 0 1) (mcofactor m 0 2) (mcofactor m 0 3)
               (mcofactor m 1 0) (mcofactor m 1 1) (mcofactor m 1 2) (mcofactor m 1 3)
               (mcofactor m 2 0) (mcofactor m 2 1) (mcofactor m 2 2) (mcofactor m 2 3)
               (mcofactor m 3 0) (mcofactor m 3 1) (mcofactor m 3 2) (mcofactor m 3 3)))
    (matn (let ((r (matn (%rows m) (%cols m))))
            (dotimes (y (%rows m) r)
              (dotimes (x (%cols m) r)
                (setf (mcrefn r y x) (mcofactor m y x))))))))

(declaim (ftype (function (mat) mat) madj))
(define-ofun madj (m)
  (declare (inline mtranspose mcof))
  (with-fast-matcase (e m)
    (mat2 (mat (+ (e 1 1))
               (- (e 1 0))
               (- (e 0 1))
               (+ (e 0 0))))
    (mat3 (mat (- (* (e 1 1) (e 2 2))
                  (* (e 1 2) (e 2 1)))
               (- (* (e 0 2) (e 2 1))
                  (* (e 0 1) (e 2 2)))
               (- (* (e 0 1) (e 1 2))
                  (* (e 0 2) (e 1 1)))
               (- (* (e 1 2) (e 2 0))
                  (* (e 1 0) (e 2 2)))
               (- (* (e 0 0) (e 2 2))
                  (* (e 0 2) (e 2 0)))
               (- (* (e 0 2) (e 1 0))
                  (* (e 0 0) (e 1 2)))
               (- (* (e 1 0) (e 2 1))
                  (* (e 1 1) (e 2 0)))
               (- (* (e 0 1) (e 2 0))
                  (* (e 0 0) (e 2 1)))
               (- (* (e 0 0) (e 1 1))
                  (* (e 0 1) (e 1 0)))))
    (mat4 (mat (- (+ (* (e 1 1) (e 2 2) (e 3 3)) (* (e 1 2) (e 2 3) (e 3 1)) (* (e 1 3) (e 2 1) (e 3 2)))
                  (+ (* (e 1 1) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 1) (e 3 3)) (* (e 1 3) (e 2 2) (e 3 1))))
               (- (+ (* (e 0 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 1) (e 3 3)) (* (e 0 3) (e 2 2) (e 3 1)))
                  (+ (* (e 0 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 2 2) (e 3 3))))
               (- (+ (* (e 0 1) (e 1 2) (e 3 3)) (* (e 0 2) (e 1 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 3 2)))
                  (+ (* (e 0 1) (e 1 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 3 3)) (* (e 0 3) (e 1 2) (e 3 1))))
               (- (+ (* (e 0 1) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 1) (e 2 3)) (* (e 0 3) (e 1 2) (e 2 1)))
                  (+ (* (e 0 2) (e 1 3) (e 2 1)) (* (e 0 3) (e 1 1) (e 2 2)) (* (e 0 1) (e 1 2) (e 2 3))))
               (- (+ (* (e 1 0) (e 2 3) (e 3 2)) (* (e 1 2) (e 2 0) (e 3 3)) (* (e 1 3) (e 2 2) (e 3 0)))
                  (+ (* (e 1 2) (e 2 3) (e 3 0)) (* (e 1 3) (e 2 0) (e 3 2)) (* (e 1 0) (e 2 2) (e 3 3))))
               (- (+ (* (e 0 0) (e 2 2) (e 3 3)) (* (e 0 3) (e 2 0) (e 3 2)) (* (e 0 2) (e 2 3) (e 3 0)))
                  (+ (* (e 0 0) (e 2 3) (e 3 2)) (* (e 0 2) (e 2 0) (e 3 3)) (* (e 0 3) (e 2 2) (e 3 0))))
               (- (+ (* (e 0 0) (e 1 3) (e 3 2)) (* (e 0 3) (e 1 2) (e 3 0)) (* (e 0 2) (e 1 0) (e 3 3)))
                  (+ (* (e 0 2) (e 1 3) (e 3 0)) (* (e 0 3) (e 1 0) (e 3 2)) (* (e 0 0) (e 1 2) (e 3 3))))
               (- (+ (* (e 0 0) (e 1 2) (e 2 3)) (* (e 0 3) (e 1 0) (e 2 2)) (* (e 0 2) (e 1 3) (e 2 0)))
                  (+ (* (e 0 0) (e 1 3) (e 2 2)) (* (e 0 2) (e 1 0) (e 2 3)) (* (e 0 3) (e 1 2) (e 2 0))))
               (- (+ (* (e 1 0) (e 2 1) (e 3 3)) (* (e 1 3) (e 2 0) (e 3 1)) (* (e 1 1) (e 2 3) (e 3 0)))
                  (+ (* (e 1 0) (e 2 3) (e 3 1)) (* (e 1 1) (e 2 0) (e 3 3)) (* (e 1 3) (e 2 1) (e 3 0))))
               (- (+ (* (e 0 0) (e 2 3) (e 3 1)) (* (e 0 3) (e 2 1) (e 3 0)) (* (e 0 1) (e 2 0) (e 3 3)))
                  (+ (* (e 0 1) (e 2 3) (e 3 0)) (* (e 0 3) (e 2 0) (e 3 1)) (* (e 0 0) (e 2 1) (e 3 3))))
               (- (+ (* (e 0 0) (e 1 1) (e 3 3)) (* (e 0 3) (e 1 0) (e 3 1)) (* (e 0 1) (e 1 3) (e 3 0)))
                  (+ (* (e 0 0) (e 1 3) (e 3 1)) (* (e 0 1) (e 1 0) (e 3 3)) (* (e 0 3) (e 1 1) (e 3 0))))
               (- (+ (* (e 0 0) (e 1 3) (e 2 1)) (* (e 0 3) (e 1 1) (e 2 0)) (* (e 0 1) (e 1 0) (e 2 3)))
                  (+ (* (e 0 1) (e 1 3) (e 2 0)) (* (e 0 3) (e 1 0) (e 2 1)) (* (e 0 0) (e 1 1) (e 2 3))))
               (- (+ (* (e 1 0) (e 2 2) (e 3 1)) (* (e 1 2) (e 2 1) (e 3 0)) (* (e 1 1) (e 2 0) (e 3 2)))
                  (+ (* (e 1 1) (e 2 2) (e 3 0)) (* (e 1 2) (e 2 0) (e 3 1)) (* (e 1 0) (e 2 1) (e 3 2))))
               (- (+ (* (e 0 0) (e 2 1) (e 3 2)) (* (e 0 2) (e 2 0) (e 3 1)) (* (e 0 1) (e 2 2) (e 3 0)))
                  (+ (* (e 0 0) (e 2 2) (e 3 1)) (* (e 0 1) (e 2 0) (e 3 2)) (* (e 0 2) (e 2 1) (e 3 0))))
               (- (+ (* (e 0 0) (e 1 2) (e 3 1)) (* (e 0 2) (e 1 1) (e 3 0)) (* (e 0 1) (e 1 0) (e 3 2)))
                  (+ (* (e 0 1) (e 1 2) (e 3 0)) (* (e 0 2) (e 1 0) (e 3 1)) (* (e 0 0) (e 1 1) (e 3 2))))
               (- (+ (* (e 0 0) (e 1 1) (e 2 2)) (* (e 0 2) (e 1 0) (e 2 1)) (* (e 0 1) (e 1 2) (e 2 0)))
                  (+ (* (e 0 0) (e 1 2) (e 2 1)) (* (e 0 1) (e 1 0) (e 2 2)) (* (e 0 2) (e 1 1) (e 2 0))))))
    (matn
     (mtranspose (mcof m)))))

;; pivotized matrix, pivot matrix, number of swaps.
(declaim (ftype (function (mat) (values mat mat mat-dim)) mpivot))
(define-ofun mpivot (m)
  (assert (= (mrows m) (mcols m)))
  (let* ((c (mrows m))
         (r (mcopy m))
         (p (meye c))
         (s 0))
    (declare (type mat-dim s))
    (with-fast-matrefs ((e r c))
      (dotimes (i c (values r p s))
        (let ((index 0) (max #.(ensure-float 0)))
          (loop for j from i below c
                for el = (abs (e j i))
                do (when (< max el)
                     ;; Make sure we don't accidentally introduce zeroes
                     ;; into the diagonal by swapping!
                     (when (/= 0 (e i j))
                       (setf max el)
                       (setf index j))))
          (when (= 0 max)
            (error "The matrix~%~a~%is singular in column ~a. A pivot cannot be constructed for it."
                   (write-matrix m NIL) i))
          ;; Non-diagonal means we swap. Record.
          (when (/= i index)
            (setf s (1+ s))
            (nmswap-row p i index)
            (nmswap-row r i index)))))))

(declaim (ftype (function (mat &optional boolean) (values mat mat mat-dim)) mlu))
(define-ofun mlu (m &optional (pivot T))
  (etypecase m
    (mat2 (with-fast-matref (e m 2)
            (cond ((or (/= 0 (e 0 0)) (not pivot))
                   (values (mat (e 0 0) (e 0 1) (/ (e 1 0) (e 0 0))
                                (- (e 1 1) (/ (* (e 0 1) (e 1 0)) (e 0 0))))
                           (mat 1 0 0 1)
                           0))
                  ((/= 0 (e 1 0))
                   (values (mat (e 1 0) (e 1 1)
                                (/ (e 0 0) (e 1 0)) (- (e 0 1) (/ (* (e 0 0) (e 1 1)) (e 1 0))))
                           (mat 0 1 1 0)
                           1))
                  (T
                   (error "The matrix~%~a~%is singular."
                          (write-matrix m NIL))))))
    ;; Not worth the pain to inline it anymore. Just do the generic variant.
    ;; We're using the Crout method for LU decomposition.
    ;; See https://en.wikipedia.org/wiki/Crout_matrix_decomposition
    (mat
     (let* ((lu (mcopy m))
            (n (mcols m))
            (p (meye n))
            (s 0)
            (scale (make-array n :element-type 'float-type :initial-element #.(ensure-float 0))))
       (declare (type mat-dim s))
       (with-fast-matref (lu LU n)
         ;; Discover the largest element and save the scaling.
         (loop for i from 0 below n
               for big = #.(ensure-float 0)
               do (loop for j from 0 below n
                        for temp = (abs (lu i j))
                        do (if (< big temp) (setf big temp)))
                  (when (= 0 big)
                    (error "The matrix is singular in ~a:~%~a" i
                           (write-matrix lu NIL)))
                  (setf (aref scale i) big))
         ;; Time to Crout it up.
         (dotimes (j n (values lu p s))
           ;; Part A sans diag
           (loop for i from 0 below j
                 for sum of-type #.*float-type* = (lu i j)
                 do (loop for k from 0 below i
                          do (decf sum (* (lu i k) (lu k j))))
                    (setf (lu i j) sum))
           (let ((imax j))
             ;; Diag + pivot search
             (loop with big = #.(ensure-float 0)
                   for i from j below n
                   for sum of-type #.*float-type* = (lu i j)
                   do (loop for k from 0 below j
                            do (decf sum (* (lu i k) (lu k j))))
                      (setf (lu i j) sum)
                      (when pivot
                        (let ((temp (* (abs sum) (aref scale i))))
                          (when (<= big temp)
                            (setf big temp)
                            (setf imax i)))))
             ;; Pivot swap
             (unless (= j imax)
               (incf s)
               (nmswap-row lu imax j)
               (nmswap-row p  imax j)
               (setf (aref scale imax) (aref scale j)))
             ;; Division
             (when (< j (1- n))
               (let ((div (/ (lu j j))))
                 (loop for i from (1+ j) below n
                       do (setf (lu i j) (* (lu i j) div))))))))))))

(declaim (ftype (function (vec3) mat4) mtranslation))
(define-ofun mtranslation (v)
  (mat 1 0 0 (vx3 v)
       0 1 0 (vy3 v)
       0 0 1 (vz3 v)
       0 0 0 1))

(declaim (ftype (function (vec3) mat4) mscaling))
(define-ofun mscaling (v)
  (mat (vx3 v) 0 0 0
       0 (vy3 v) 0 0
       0 0 (vz3 v) 0
       0 0 0       1))

(declaim (inline %mrotation))
(defun %mrotation (arr v angle)
  (declare (type (simple-array float-type (16)) arr))
  (declare (optimize speed (safety 0)))
  ;; https://joombig.com/sqlc/3D-Rotation-Algorithm-about-arbitrary-axis-with-CC-code-tutorials-advance
  (let* ((angle (ensure-float angle))
         (c (cos angle))
         (s (sin angle)))
    (macrolet ((%mat (&rest els)
                 `(progn ,@(loop for el in els
                                 for i from 0
                                 collect `(setf (aref arr ,i) (ensure-float ,el))))))
      (cond ((v= +vx+ v)
             (%mat 1 0 0 0
                   0 c (- s) 0
                   0 s c 0
                   0 0 0 1))
            ((v= +vy+ v)
             (%mat c 0 s 0
                   0 1 0 0
                   (- s) 0 c 0
                   0 0 0 1))
            ((v= +vz+ v)
             (%mat c (- s) 0 0
                   s c 0 0
                   0 0 1 0
                   0 0 0 1))
            (T
             (with-vec3 (x y z) v
               (let* ((1-c (- 1 c))
                      (u2 (expt x 2))
                      (v2 (expt y 2))
                      (w2 (expt z 2))
                      (l (+ u2 v2 w2))
                      (sqrtl (sqrt l)))
                 (%mat (/ (+ u2 (* (+ v2 w2) c)) l)        (/ (- (* x y 1-c) (* z sqrtl s)) l) (/ (+ (* x z 1-c) (* y sqrtl s)) l) 0
                       (/ (+ (* x y 1-c) (* z sqrtl s)) l) (/ (+ v2 (* (+ u2 w2) c)) l)        (/ (- (* y z 1-c) (* x sqrtl s)) l) 0
                       (/ (- (* x z 1-c) (* y sqrtl s)) l) (/ (+ (* y z 1-c) (* x sqrtl s)) l) (/ (+ w2 (* (+ u2 v2) c)) l)        0
                       0                                   0                                   0                                   1))))))))

(declaim (ftype (function (vec3 real) mat4) mrotation))
(define-ofun mrotation (v angle)
  (let ((mat (mat4)))
    (%mrotation (marr4 mat) v angle)
    mat))

(declaim (ftype (function (vec3 vec3 vec3) mat4) mlookat))
(define-ofun mlookat (eye target up)
  (let* ((z (nvunit (v- eye target)))
         (x (if (and (= (abs (vx z)) (abs (vx up)))
                     (= (abs (vy z)) (abs (vy up)))
                     (= (abs (vz z)) (abs (vz up))))
                (nvunit (vec (float-sign (vz z) (vx z))
                             (float-sign (vz z) (vy z))
                             (- (float-sign (+ (abs (vx z)) (abs (vy z))) (vz z)))))
                (nvunit (vc up z))))
         (y (vc z x)))
    (mat (vx3 x) (vy3 x) (vz3 x) (- (v. x eye))
         (vx3 y) (vy3 y) (vz3 y) (- (v. y eye))
         (vx3 z) (vy3 z) (vz3 z) (- (v. z eye))
         #.(ensure-float 0) #.(ensure-float 0) #.(ensure-float 0) #.(ensure-float 1))))

(declaim (ftype (function (real real real real real real) mat4) mfrustum))
(define-ofun mfrustum (left right bottom top near far)
  (with-floats ((f2 2) (f-2 -2) (f0 0) (f-1 -1)
                (l left) (r right) (b bottom) (u top) (n near) (f far))
    (mat (/ (* f2 n) (- r l)) f0                   (/ (+ r l) (- r l))     f0
         f0                   (/ (* f2 n) (- u b)) (/ (+ u b) (- u b))     f0
         f0                   f0                   (- (/ (+ f n) (- f n))) (/ (* f-2 f n) (- f n))
         f0                   f0                   f-1                     f0)))

(declaim (ftype (function (real real real real) mat4) mperspective))
(define-ofun mperspective (fovy aspect near far)
  ;; http://nehe.gamedev.net/article/replacement_for_gluperspective/21002/
  (with-floats ((fpi PI) (f360 360) (fovy fovy) (aspect aspect) (near near) (far far))
    (let* ((fh (* (the #.*float-type* (tan (* (/ fovy f360) fpi))) near))
           (fw (* fh aspect)))
      (mfrustum (- fw) fw (- fh) fh near far))))

(declaim (ftype (function (real real real real real real) mat4) mortho))
(define-ofun mortho (left right bottom top near far)
  (with-floats ((f2 2) (f-2 -2) (f0 0) (f1 1)
                (r right) (l left) (b bottom) (u top) (n near) (f far))
    (mat (/ f2 (- r l)) f0             f0              (- (/ (+ r l) (- r l)))
         f0             (/ f2 (- u b)) f0              (- (/ (+ u b) (- u b)))
         f0             f0             (/ f-2 (- f n)) (- (/ (+ f n) (- f n)))
         f0             f0             f0              f1)))

(declaim (ftype (function (mat4 vec3) mat4) nmtranslate))
(define-ofun nmtranslate (m v)
  (with-fast-matref (f m 4)
    (setf (f 0 3) (+ (f 0 3) (* (f 0 0) (vx3 v)) (* (f 0 1) (vy3 v)) (* (f 0 2) (vz3 v))))
    (setf (f 1 3) (+ (f 1 3) (* (f 1 0) (vx3 v)) (* (f 1 1) (vy3 v)) (* (f 1 2) (vz3 v))))
    (setf (f 2 3) (+ (f 2 3) (* (f 2 0) (vx3 v)) (* (f 2 1) (vy3 v)) (* (f 2 2) (vz3 v))))
    (setf (f 3 3) (+ (f 3 3) (* (f 3 0) (vx3 v)) (* (f 3 1) (vy3 v)) (* (f 3 2) (vz3 v))))
    m))

(declaim (ftype (function (mat4 vec3) mat4) nmscale))
(define-ofun nmscale (m v)
  (with-fast-matref (f m 4)
    (setf (f 0 0) (* (vx3 v) (f 0 0)))
    (setf (f 0 1) (* (vy3 v) (f 0 1)))
    (setf (f 0 2) (* (vz3 v) (f 0 2)))
    (setf (f 1 0) (* (vx3 v) (f 1 0)))
    (setf (f 1 1) (* (vy3 v) (f 1 1)))
    (setf (f 1 2) (* (vz3 v) (f 1 2)))
    (setf (f 2 0) (* (vx3 v) (f 2 0)))
    (setf (f 2 1) (* (vy3 v) (f 2 1)))
    (setf (f 2 2) (* (vz3 v) (f 2 2)))
    (setf (f 3 0) (* (vx3 v) (f 3 0)))
    (setf (f 3 1) (* (vy3 v) (f 3 1)))
    (setf (f 3 2) (* (vz3 v) (f 3 2)))
    m))

(declaim (ftype (function (mat4 vec3 real) mat4) nmrotate))
(define-ofun nmrotate (m v angle)
  (declare (inline mrotation))
  (let ((arr (make-array 16 :element-type '#.*float-type*)))
    (declare (dynamic-extent arr))
    (%mrotation arr v angle)
    (let ((mat (%mat4 arr)))
      (declare (dynamic-extent mat))
      (values (nm* m mat)))))

(declaim (ftype (function (mat4 vec3 vec3 vec3) mat4) nmlookat))
(define-ofun nmlookat (m eye target up)
  (declare (inline mlookat))
  (nm* m (mlookat eye target up)))

(define-ofun m1norm (m)
  (let ((max #.(ensure-float 0)))
    (with-fast-matref (e m (mcols m))
      (dotimes (j (mcols m) max)
        (let ((col (loop for i from 0 below (mrows m)
                         sum (abs (e i j)))))
          (when (< max col)
            (setf max col)))))))

(define-ofun minorm (m)
  (let ((max #.(ensure-float 0)))
    (with-fast-matref (e m (mcols m))
      (dotimes (i (mrows m) max)
        (let ((col (loop for j from 0 below (mcols m)
                         sum (abs (e i j)))))
          (when (< max col)
            (setf max col)))))))

(define-ofun m2norm (m)
  (let ((sum #.(ensure-float 0)))
    (declare (type float-type sum))
    (do-mat-index (i el m (sqrt sum))
      (incf sum (expt el 2)))))

(declaim (ftype (function (mat) (values mat mat)) mqr))
(define-ofun mqr (mat)
  (let* ((m (mrows mat))
         (n (mcols mat))
         (Q (meye m))
         (R (mcopy mat))
         (G (meye m)))
    (with-fast-matrefs ((g G m)
                        (r R m))
      (dotimes (j n (values Q R))
        (loop for i downfrom (1- m) above j
              for a of-type #.*float-type* = (r (1- i) j)
              for b of-type #.*float-type* = (r     i  j)
              for c of-type #.*float-type* = #.(ensure-float 0)
              for s of-type #.*float-type* = #.(ensure-float 0)
              do (cond ((= 0 b) (setf c #.(ensure-float 1)))
                       ((= 0 a) (setf s #.(ensure-float 1)))
                       ((< (abs a) (abs b))
                        (let ((r (/ a b)))
                          (setf s (/ (sqrt (1+ (* r r)))))
                          (setf c (* s r))))
                       (T
                        (let ((r (/ b a)))
                          (setf c (/ (sqrt (1+ (* r r)))))
                          (setf s (* c r)))))
                 (setf (g (1- i) (1- i)) c
                       (g (1- i)     i)  (- s)
                       (g     i  (1- i)) s
                       (g     i      i)  c)
                 (n*m (mtranspose G) R)
                 (nm* Q G)
                 (setf (g (1- i) (1- i)) #.(ensure-float 1)
                       (g (1- i)     i)  #.(ensure-float 0)
                       (g     i  (1- i)) #.(ensure-float 0)
                       (g     i      i)  #.(ensure-float 1)))))))

(declaim (ftype (function (mat &optional (integer 0)) list) meigen))
(defun meigen (m &optional (iterations 50))
  (multiple-value-bind (Q R) (mqr m)
    (loop repeat iterations
          do (multiple-value-bind (Qn Rn)
                 (mqr (nm* R Q))
               (setf Q Qn)
               (setf R Rn)))
    (mdiag (nm* R Q))))

(declaim (inline nmswap-row))
(declaim (ftype (function (mat mat-dim mat-dim) mat) nmswap-row))
(define-ofun nmswap-row (m k l)
  (let ((s (mcols m)))
    (with-fast-matref (e m s)
      (dotimes (i s m)
        (rotatef (e k i) (e l i))))))

(declaim (inline mswap-row))
(declaim (ftype (function (mat mat-dim mat-dim) mat) mswap-row))
(define-ofun mswap-row (m k l)
  (nmswap-row (mcopy m) k l))

(declaim (inline nmswap-col))
(declaim (ftype (function (mat mat-dim mat-dim) mat) nmswap-col))
(define-ofun nmswap-col (m k l)
  (let ((s (mcols m)))
    (with-fast-matref (e m s)
      (dotimes (i s m)
        (rotatef (e i k) (e i l))))))

(declaim (inline mswap-col))
(declaim (ftype (function (mat mat-dim mat-dim) mat) mswap-col))
(define-ofun mswap-col (m k l)
  (nmswap-col (mcopy m) k l))

(declaim (ftype (function (mat) list) mdiag))
(defun mdiag (m)
  (let ((values ()))
    (do-mat-diag (i el m (nreverse values))
      (push el values))))

(declaim (ftype (function (mat mat-dim mat-dim mat-dim mat-dim) mat) mblock))
(defun mblock (m y1 x1 y2 x2)
  (let ((r (matn (- y2 y1) (- x2 x1))))
    (with-fast-matref (e m (mcols m))
      (with-fast-matref (f r (mcols r))
        (dotimes (i (mrows r) r)
          (dotimes (j (mcols r))
            (setf (f i j) (e (+ i y1) (+ j x1)))))))))

(declaim (ftype (function (mat mat-dim) mat) mtop))
(defun mtop (m n)
  (declare (inline mblock))
  (mblock m 0 0 n (mcols m)))

(declaim (ftype (function (mat mat-dim) mat) mbottom))
(defun mbottom (m n)
  (declare (inline mblock))
  (mblock m (- (mrows m) n) 0 (mrows m) (mcols m)))

(declaim (ftype (function (mat mat-dim) mat) mleft))
(defun mleft (m n)
  (declare (inline mblock))
  (mblock m 0 0 (mrows m) n))

(declaim (ftype (function (mat mat-dim) mat) mright))
(defun mright (m n)
  (declare (inline mblock))
  (mblock m 0 (- (mcols m) n) (mrows m) (mcols m)))
