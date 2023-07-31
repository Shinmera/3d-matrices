#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.matrices)

(defmacro define-2mat-dispatch (op)
  `(define-templated-dispatch ,(compose-name NIL '!2m op) (x a b)
     ((mat-type 0 #(0 1)) smatop ,op <t>)
     ((mat-type 0 real) smatop ,op real)
     ((mat-type 0 0) 2matop ,op)))

(defmacro define-2mat-*-dispatch ()
  `(define-templated-dispatch !2m* (x a b)
     ((mat-type 0 #(0 1)) smatop * <t>)
     ((mat-type 0 real) smatop * real)
     ((mat-type 0 0) 2matop *)
     ;; Extra handling for vectors.
     ,@(loop for instance in (instances 'mat-type)
             for (<s> <t>) = (template-arguments instance)
             append (loop for <vs> in '(2 3 4)
                          for vec-type = (type-instance 'vec-type <vs> <t>)
                          collect `((,(lisp-type vec-type) ,(lisp-type instance) 0)
                                    m*v ,<vs> ,<s> ,<t>)))))

(defmacro define-1mat-dispatch (name op &rest template-args)
  `(define-templated-dispatch ,name (x a)
     ((mat-type 0) ,op ,@template-args)))

(defmacro define-matcomp-dispatch (op &optional (comb 'and))
  `(define-templated-dispatch ,(compose-name NIL '2m op) (a b)
     ((mat-type #(0 1)) smatreduce ,comb ,op <t>)
     ((mat-type real) smatreduce ,comb ,op real)
     ((mat-type 0) 2matreduce ,comb ,op)))

(defmacro define-mat-reductor (name 2-op &optional 1-op)
  `(progn
     (defun ,name (target value &rest values)
       (cond ((null values)
              ,(if 1-op
                   `(,1-op target value)
                   `(m<- target value)))
             ((null (cdr values))
              (,2-op target value (first values)))
             (T
              (,2-op target value (first values))
              (dolist (value (rest values) target)
                (,2-op target target value)))))

     (define-compiler-macro ,name (target value &rest values)
       (cond ((null values)
              ,(if 1-op
                   ``(,',1-op ,target ,value)
                   ``(m<- ,target ,value)))
             ((null (cdr values))
              `(,',2-op ,target ,value ,(first values)))
             (T
              (let ((targetg (gensym "TARGET")))
                `(let ((,targetg ,target))
                   (,',2-op ,targetg ,value ,(first values))
                   ,@(loop for value in (rest values)
                           collect `(,',2-op ,targetg ,targetg ,value)))))))))

(defmacro define-value-reductor (name 2-op comb identity)
  `(progn
     (defun ,name (value &rest values)
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              (,2-op value (first values)))
             (T
              (let* ((previous (first values))
                     (result (,2-op value previous)))
                (dolist (value (rest values) result)
                  (setf result (,comb result (,2-op previous value)))
                  (setf previous value))))))

     (define-compiler-macro ,name (value &rest values)
       (cond ((null values)
              ,identity)
             ((null (cdr values))
              `(,',2-op ,value ,(first values)))
             (T
              (let ((previous (gensym "PREVIOUS"))
                    (next (gensym "NEXT")))
                `(let ((,previous ,value))
                   (,',comb ,@(loop for value in values
                                    collect `(let ((,next ,value))
                                               (prog1 (,',2-op ,previous ,next)
                                                 (setf ,previous ,next))))))))))))


(defmacro define-pure-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     `(,',func (mzero ,,(first args)) ,,@(v::lambda-list-variables args))))

(defmacro define-modifying-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     `(,',func ,,(first args) ,,@(v::lambda-list-variables args))))

(defmacro define-simple-alias (name args &optional (func (compose-name NIL '! name)))
  `(progn (define-pure-alias ,name ,args ,func)
          (define-modifying-alias ,(compose-name NIL 'n name) ,args ,func)))

(defmacro define-rest-alias (name args &optional (func (compose-name NIL '! name)))
  (let ((vars (v::lambda-list-variables args))
        (nname (compose-name NIL 'n name)))
    `(progn
       (defun ,name ,args
         (apply #',func (mzero ,(first args)) ,@vars))
       (defun ,nname ,args
         (apply #',func ,(first args) ,@vars))
       
       (define-compiler-macro ,name ,args
         `(let ,(list ,@(loop for var in (butlast vars)
                              collect `(list ',var ,var)))
            (,',func (mzero ,',(first args)) ,',@(butlast vars) ,@,(car (last vars)))))
       (define-compiler-macro ,nname ,args
         `(let ,(list ,@(loop for var in (butlast vars)
                              collect `(list ',var ,var)))
            (,',func ,',(first args) ,',@(butlast vars) ,@,(car (last vars))))))))

(defmacro define-constructor (name initializer)
  `(define-type-dispatch ,name (x)
     ,@(loop for instance in (instances 'mat-type)
             collect (if (eql 'n (first (template-arguments instance)))
                         `((,(lisp-type instance)) ,(lisp-type instance) (,initializer (,(lisp-type instance) (mrows x) (mcols x))))
                         `((,(lisp-type instance)) ,(lisp-type instance) (,initializer (,(lisp-type instance))))))
     (((eql 2)) mat2 (,initializer (mat2)))
     (((eql 3)) mat3 (,initializer (mat3)))
     (((eql 4)) mat4 (,initializer (mat4)))
     ((integer) matn (,initializer (matn x x)))))

(defmacro define-vec-return (name args)
  (let ((nname (compose-name NIL 'n name)))
    `(progn
       (define-templated-dispatch ,nname (x ,@(mapcar #'first args))
         ((#'(matching-vec 1) ,@(mapcar #'second args)) ,name))
       
       (define-templated-dispatch ,name ,(mapcar #'first args)
         (,(mapcar #'second args) (,name) (mvec ,(first (first args))) ,@(mapcar #'first args))))))

(define-dependent-dispatch-type matching-vec (types i ref)
  (handler-case (apply #'type-instance 'vec-type (template-arguments (nth ref types)))
    (error () NIL)))

(define-dependent-dispatch-type lower-vec (types i ref)
  (handler-case (destructuring-bind (<s> <t>) (template-arguments (nth ref types))
                  (apply #'type-instance 'vec-type (1- <s>) <t>))
    (error () NIL)))

(define-dependent-dispatch-type matching-array (types i ref)
  (destructuring-bind (<s> <t>) (template-arguments (nth ref types))
    (declare (ignore <s>))
    `(simple-array ,<t> (*))))

(define-2mat-dispatch +)
(define-2mat-dispatch -)
(define-2mat-dispatch /)
(define-2mat-dispatch min)
(define-2mat-dispatch max)

(define-2mat-*-dispatch)

(define-matcomp-dispatch =)
(define-matcomp-dispatch ~=)
(define-matcomp-dispatch /= or)
(define-matcomp-dispatch <)
(define-matcomp-dispatch <=)
(define-matcomp-dispatch >)
(define-matcomp-dispatch >=)

(define-templated-dispatch mvec (a)
  ((mat-type) mvec))

(define-templated-dispatch mcopy (a)
  ((mat-type) copy))

(define-1mat-dispatch m<- 1matop identity)

(define-1mat-dispatch !1m- 1matop -)
(define-1mat-dispatch !1m/ 1matop /)

(define-templated-dispatch !mapply (x m f)
  ((mat-type 0 function) mapply)
  ((mat-type 0 symbol) (mapply) x m (fdefinition f)))

(define-1mat-dispatch !mcof mcof)
(define-1mat-dispatch !minv minv)
(define-1mat-dispatch !minv-affine minv-affine)
(define-1mat-dispatch !mtranspose mtranspose)
(define-templated-dispatch !mswap-row (x m r1 r2)
  ((mat-type 0 dimension dimension) mswap-row))
(define-templated-dispatch !mswap-col (x m c1 c2)
  ((mat-type 0 dimension dimension) mswap-col))
(define-templated-dispatch !mrow (r m ri)
  ((#'(matching-array 1) mat-type dimension) mrow)
  ((#'(matching-vec 1) mat-type dimension) (mrow) (varr r) m ri)
  ((null mat-type dimension) (mrow) (make-array (mcols m) :element-type (array-element-type (marr m))) m ri))
(define-templated-dispatch !mcol (r m ci)
  ((#'(matching-array 1) mat-type dimension) mcol)
  ((#'(matching-vec 1) mat-type dimension) (mcol) (varr r) m ci)
  ((null mat-type dimension) (mcol) (make-array (mrows m) :element-type (array-element-type (marr m))) m ci))
(define-templated-dispatch !mdiag (r m)
  ((#'(matching-array 1) mat-type) mdiag)
  ((#'(matching-vec 1) mat-type) (mdiag) (varr r) m)
  ((null mat-type dimension) (mdiag) (make-array (min (mcols m) (mrows m)) :element-type (array-element-type (marr m))) m))

(define-mat-reductor !m+ !2m+)
(define-mat-reductor !m* !2m*)
(define-mat-reductor !m- !2m- !1m-)
(define-mat-reductor !m/ !2m/ !1m/)
(define-mat-reductor !mmin !2mmin)
(define-mat-reductor !mmax !2mmax)

(define-templated-dispatch !mzero (x)
  ((mat-type) 0matop zero))
(define-templated-dispatch !meye (x)
  ((mat-type) 0matop eye))
(define-templated-dispatch !mrand (x)
  ((mat-type) 0matop rand))

(define-value-reductor m= 2m= and T)
(define-value-reductor m~= 2m~= and T)
(define-value-reductor m/= 2m/= and T)
(define-value-reductor m< 2m< and T)
(define-value-reductor m<= 2m<= and T)
(define-value-reductor m> 2m> and T)
(define-value-reductor m>= 2m>= and T)

(define-pure-alias mapply (m f) !mapply)
(define-modifying-alias mapplyf (m f) !mapply)
(define-simple-alias mcof (m))
(define-simple-alias minv (m))
(define-simple-alias minv-affine (m))
(define-simple-alias mtranspose (m))
(define-simple-alias mswap-row (m r1 r2))
(define-simple-alias mswap-col (m c1 c2))
(define-alias mrow (m ri) `(!mrow NIL ,m ,ri))
(define-alias mcol (m ri) `(!mcol NIL ,m ,ri))
(define-alias mdiag (m) `(!mdiag NIL ,m))

(define-templated-dispatch mminor (m y x)
  ((mat-type dimension dimension) mminor))
(define-templated-dispatch mdet (m)
  ((mat-type) mdet))
(define-templated-dispatch mtrace (m)
  ((mat-type) mtrace))
(define-templated-dispatch m1norm (m)
  ((mat-type) m1norm))
(define-templated-dispatch minorm (m)
  ((mat-type) m1norm))
(define-templated-dispatch m2norm (m)
  ((mat-type) m2norm))

(define-rest-alias m+ (m &rest others))
(define-rest-alias m- (m &rest others))
(define-rest-alias m* (m &rest others))
(define-rest-alias m/ (m &rest others))
(define-rest-alias mmin (m &rest others))
(define-rest-alias mmax (m &rest others))

(defun n*m (&rest others)
  (apply #'!m* (car (last others)) others))

(define-compiler-macro n*m (&rest others)
  `(!m* ,(car (last others)) ,@others))

(define-templated-dispatch nmtranslate (x v)
  ((mat-type #'(lower-vec 0)) mtranslate))
(define-templated-dispatch nmscale (x v)
  ((mat-type #'(lower-vec 0)) mscale))
(define-templated-dispatch nmrotate (x v angle)
  ((mat-type #'(lower-vec 0) #(0 1)) mrotate))

(define-templated-dispatch nmtranslation (x v)
  ((mat-type #'(matching-vec 0)) mtranslation))
(define-templated-dispatch nmscaling (x v)
  ((mat-type #'(matching-vec 0)) mscaling))
(define-templated-dispatch nmrotation (x v angle)
  ((mat-type #'(matching-vec 0) #(0 1)) mrotation))
(define-templated-dispatch nmlookat (x eye target up)
  ((mat-type #'(matching-vec 0) 1 1) mlookat))
(define-templated-dispatch nmfrustum (x l r b u n f)
  ((mat-type #(0 1) 1 1 1 1 1 1) mfrustum))
(define-templated-dispatch nmperspective (x fovy aspect n f)
  ((mat-type #(0 1) 1 1 1) mperspective))
(define-templated-dispatch nmortho (x l r b u n f)
  ((mat-type #(0 1) 1 1 1 1 1 1) mortho))
(define-templated-dispatch nmperspective (x fovy aspect near far)
  ((mat-type #(0 1) 1 1 1) mperspective))

(define-type-dispatch mtranslation (v)
  #-3d-vectors-no-f32 ((vec2) mat3 (mtranslation/3/f32 (mat3) v))
  #-3d-vectors-no-f64 ((dvec2) dmat3 (mtranslation/3/f64 (dmat3) v))
  #-3d-vectors-no-u32 ((uvec2) umat3 (mtranslation/3/u32 (umat3) v))
  #-3d-vectors-no-i32 ((ivec2) imat3 (mtranslation/3/i32 (imat3) v))
  #-3d-vectors-no-f32 ((vec3) mat4 (mtranslation/4/f32 (mat4) v))
  #-3d-vectors-no-f64 ((dvec3) dmat4 (mtranslation/4/f64 (dmat4) v))
  #-3d-vectors-no-u32 ((uvec3) umat4 (mtranslation/4/u32 (umat4) v))
  #-3d-vectors-no-i32 ((ivec3) imat4 (mtranslation/4/i32 (imat4) v)))

(define-type-dispatch mscaling (v)
  #-3d-vectors-no-f32 ((vec2) mat3 (mscaling/3/f32 (mat3) v))
  #-3d-vectors-no-f64 ((dvec2) dmat3 (mscaling/3/f64 (dmat3) v))
  #-3d-vectors-no-u32 ((uvec2) umat3 (mscaling/3/u32 (umat3) v))
  #-3d-vectors-no-i32 ((ivec2) imat3 (mscaling/3/i32 (imat3) v))
  #-3d-vectors-no-f32 ((vec3) mat4 (mscaling/4/f32 (mat4) v))
  #-3d-vectors-no-f64 ((dvec3) dmat4 (mscaling/4/f64 (dmat4) v))
  #-3d-vectors-no-u32 ((uvec3) umat4 (mscaling/4/u32 (umat4) v))
  #-3d-vectors-no-i32 ((ivec3) imat4 (mscaling/4/i32 (imat4) v)))

(define-type-dispatch mrotation (v angle)
  #-3d-vectors-no-f32 ((null f32) mat2 (mrotation/2/f32 (mat2) +vx+ angle))
  #-3d-vectors-no-f64 ((null f64) dmat2 (mrotation/2/f64 (dmat2) +vx+ angle))
  #-3d-vectors-no-f32 ((vec3 f32) mat4 (mrotation/4/f32 (mat4) v angle))
  #-3d-vectors-no-f64 ((dvec3 f64) dmat4 (mrotation/4/f64 (dmat4) v angle)))

(define-type-dispatch mlookat (eye target up)
  #-3d-vectors-no-f32 ((vec3 vec3 vec3) mat4 (mlookat/4/f32 (mat4) eye target up))
  #-3d-vectors-no-f64 ((dvec3 dvec3 dvec3) dmat4 (mlookat/4/f64 (dmat4) eye target up)))

(define-type-dispatch mfrustum (l r b u n f)
  #-3d-vectors-no-f32 ((f32 f32 f32 f32 f32 f32) mat4 (mfrustum/4/f32 (mat4) l r b u n f))
  #-3d-vectors-no-f64 ((f64 f64 f64 f64 f64 f64) dmat4 (mfrustum/4/f64 (dmat4) l r b u n f)))

(define-type-dispatch mperspective (fovy aspect n f)
  #-3d-vectors-no-f32 ((f32 f32 f32 f32) mat4 (mperspective/4/f32 (mat4) fovy aspect n f))
  #-3d-vectors-no-f64 ((f64 f64 f64 f64) dmat4 (mperspective/4/f64 (dmat4) fovy aspect n f)))

(define-type-dispatch mortho (l r b u n f)
  #-3d-vectors-no-f32 ((f32 f32 f32 f32 f32 f32) mat4 (mortho/4/f32 (mat4) l r b u n f))
  #-3d-vectors-no-f64 ((f64 f64 f64 f64 f64 f64) dmat4 (mortho/4/f64 (dmat4) l r b u n f)))

(define-constructor meye !meye)
(define-constructor mrand !mrand)
(define-constructor mzero !mzero)

(declaim (ftype (function (*mat) (values *mat *mat mat-dim &optional)) mpivot))
(defun mpivot (m)
  (assert (= (mrows m) (mcols m)))
  (let* ((c (mrows m))
         (r (mcopy m))
         (ra (marr r))
         (p (meye c))
         (s 0))
    (declare (type mat-dim s))
    (macrolet ((e (y x) `(aref ra (+ ,x (* ,y c)))))
      (dotimes (i c (values r p s))
        (let ((index 0) (max 0))
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

(declaim (ftype (function (*mat &optional boolean) (values *mat *mat dimension &optional)) mlu))
(defun mlu (m &optional (pivot T))
  ;; We're using the Crout method for LU decomposition.
  ;; See https://en.wikipedia.org/wiki/Crout_matrix_decomposition
  (let* ((lu (mcopy m))
         (n (mcols m))
         (p (meye n))
         (s 0)
         (lua (marr lu))
         (scale (make-array n :element-type (array-element-type (marr m)))))
    (declare (type mat-dim s))
    (macrolet ((lu (y x) `(aref lua (+ ,x (* ,y n)))))
      ;; Discover the largest element and save the scaling.
      (loop for i from 0 below n
            for big = 0
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
              for sum = (lu i j)
              do (loop for k from 0 below i
                       do (decf sum (* (lu i k) (lu k j))))
                 (setf (lu i j) sum))
        (let ((imax j))
          ;; Diag + pivot search
          (loop with big = 0
                for i from j below n
                for sum = (lu i j)
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
                    do (setf (lu i j) (* (lu i j) div))))))))))

(declaim (ftype (function (*mat) (values *mat *mat &optional)) mqr))
(defun mqr (mat)
  (let* ((m (mrows mat))
         (n (mcols mat))
         (Q (meye m))
         (R (mcopy mat))
         (G (meye m))
         (ra (marr r))
         (ga (marr g)))
    (macrolet ((g (y x) `(aref ga (+ ,x (* ,y m))))
               (r (y x) `(aref ra (+ ,x (* ,y m)))))
      (dotimes (j n (values Q R))
        (loop for i downfrom (1- m) above j
              for a = (r (1- i) j)
              for b = (r     i  j)
              for c = 0
              for s = 0
              do (cond ((= 0 b) (setf c 1))
                       ((= 0 a) (setf s 1))
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
                 (setf (g (1- i) (1- i)) 1
                       (g (1- i)     i)  0
                       (g     i  (1- i)) 0
                       (g     i      i)  1))))))

(declaim (ftype (function (*mat &optional (integer 0)) (values simple-array &optional)) meigen))
(defun meigen (m &optional (iterations 50))
  (multiple-value-bind (Q R) (mqr m)
    (loop repeat iterations
          do (multiple-value-bind (Qn Rn)
                 (mqr (nm* R Q))
               (setf Q Qn)
               (setf R Rn)))
    (mdiag (nm* R Q))))

(define-alias mcofactor (m y x)
  `(* (if (evenp (+ ,y ,x)) 1 -1)
      (mminor ,m ,y ,x)))

(define-alias !madj (r m)
  `(nmtranspose (!mcof ,r ,m)))

(define-alias mcof (m)
  `(!mcof (mzero ,m) ,m))

(define-alias madj (m)
  `(nmtranspose (mcof ,m)))

(define-alias mcref (m y x)
  (let ((mg (gensym "M")))
    `(let ((,mg ,m))
       (aref (marr ,mg) (+ ,x (* ,y (mcols ,mg)))))))

(define-alias (setf mcref) (value m y x)
  (let ((mg (gensym "M")))
    `(let ((,mg ,m))
       (setf (aref (marr ,mg) (+ ,x (* ,y (mcols ,mg)))) ,value))))

(define-alias miref (m i)
  `(aref (marr ,m) ,i))

(define-alias (setf miref) (value m i)
  `(setf (aref (marr ,m) ,i) ,value))

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

(defmacro with-fast-matref ((accessor mat width) &body body)
  (let ((w (gensym "WIDTH")) (arr (gensym "ARRAY"))
        (x (gensym "X")) (y (gensym "Y")))
    `(let ((,w ,width)
           (,arr (marr ,mat)))
       (declare (ignorable ,w))
       (macrolet ((,accessor (,y &optional ,x)
                    `(the ,*float-type*
                          (aref ,',arr ,(if ,x
                                            `(+ ,,x (* ,,y ,',(if (constantp width) width w)))
                                            ,y)))))
         ,@body))))
