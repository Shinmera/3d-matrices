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

(define-1mat-dispatch m<- 1matop identity)

(define-1mat-dispatch !1m- 1matop -)
(define-1mat-dispatch !1m/ 1matop /)

(define-1mat-dispatch !minv-affine minv-affine)
(define-1mat-dispatch !mtranspose mtranspose)

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

(define-dependent-dispatch-type matching-vec (types i ref)
  (handler-case (apply #'type-instance 'vec-type (template-arguments (nth ref types)))
    (error () NIL)))

(define-value-reductor m= 2m= and T)
(define-value-reductor m~= 2m~= and T)
(define-value-reductor m/= 2m/= and T)
(define-value-reductor m< 2m< and T)
(define-value-reductor m<= 2m<= and T)
(define-value-reductor m> 2m> and T)
(define-value-reductor m>= 2m>= and T)

(define-simple-alias minv-affine (m))
(define-simple-alias mtranspose (m))

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

(define-vec-return mdiag ((m mat-type)))

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

;; [x] meye
;; [x] mrand
;; [x] mzero
;; [ ] mvec
;; [x] m=
;; [x] m~=
;; [x] m/=
;; [x] m<
;; [x] m>
;; [x] m<=
;; [x] m>=
;; [x] m+
;; [x] nm+
;; [x] m-
;; [x] nm-
;; [x] m*
;; [x] nm*
;; [x] n*m
;; [x] m/
;; [x] nm/
;; [x] mdet
;; [ ] minv
;; [x] minv-affine
;; [x] mtranspose
;; [x] nmtranspose
;; [x] mtrace
;; [ ] mminor
;; [ ] mcofactor
;; [ ] mcof
;; [ ] madj
;; [ ] mpivot
;; [ ] mlu
;; [ ] mqr
;; [ ] meigen
;; [x] mrotation
;; [x] mscaling
;; [x] mrotation
;; [x] mlookat
;; [x] mfrustum
;; [x] mortho
;; [x] mperspective
;; [ ] nmtranslate
;; [ ] nmscale
;; [ ] nmrotate
;; [x] m1norm
;; [x] minorm
;; [x] m2norm
;; [ ] nmswap-row
;; [ ] nmswap-col
;; [x] mdiag
;; [ ] miref
;; [ ] mcref
;; [ ] msetf
;; [ ] mcol
;; [ ] mrow
;; [ ] mapply
;; [ ] mapplyf
;; [ ] mblock
;; [ ] mtop
;; [ ] mbottom
;; [ ] mleft
;; [ ] mright
