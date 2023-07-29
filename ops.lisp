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

(define-2mat-dispatch +)
(define-2mat-dispatch -)
(define-2mat-dispatch /)
(define-2mat-dispatch min)
(define-2mat-dispatch max)

(defmacro define-2mat-*-dispatch ()
  `(define-templated-dispatch !2m* (x a b)
     ((mat-type 0 #(0 1)) smatop * <t>)
     ((mat-type 0 real) smatop * real)
     ((mat-type 0 0) 2matop *)
     ,@(loop for instance in (instances 'mat-type)
             for (<s> <t>) = (template-arguments instance)
             for vec-type = (ignore-errors (type-instance 'vec-type <s> <t>))
             when vec-type
             collect `((,(lisp-type vec-type) ,(lisp-type instance) 0)
                       m*v ,<s> ,<s> ,<t>)
             when (eql <s> 4)
             collect `((,(lisp-type (type-instance 'vec-type 3 <t>)) ,(lisp-type instance) 0)
                       m*v 3 ,<s> ,<t>))))

(define-2mat-*-dispatch)

(define-matcomp-dispatch =)
(define-matcomp-dispatch /= or)
(define-matcomp-dispatch <)
(define-matcomp-dispatch <=)
(define-matcomp-dispatch >)
(define-matcomp-dispatch >=)

(define-1mat-dispatch m<- 1matop identity)

(define-1mat-dispatch !1m- 1matop -)
(define-1mat-dispatch !1m/ 1matop /)

(define-mat-reductor !m+ !2m+)
(define-mat-reductor !m* !2m*)
(define-mat-reductor !m- !2m- !1m-)
(define-mat-reductor !m/ !2m/ !1m/)
(define-mat-reductor !mmin !2mmin)
(define-mat-reductor !mmax !2mmax)

(define-templated-dispatch !mrand (x a var)
  ((mat-type 0 0) random))

(define-value-reductor m= 2m= and T)
(define-value-reductor m/= 2m/= and T)
(define-value-reductor m< 2m< and T)
(define-value-reductor m<= 2m<= and T)
(define-value-reductor m> 2m> and T)
(define-value-reductor m>= 2m>= and T)

(define-templated-dispatch m1norm (a)
  ((mat-type) m1norm))
(define-templated-dispatch minorm (a)
  ((mat-type) m1norm))
(define-templated-dispatch m2norm (a)
  ((mat-type) m2norm))

(define-rest-alias m+ (m &rest others))
(define-rest-alias m- (m &rest others))
(define-rest-alias m* (m &rest others))
(define-rest-alias m/ (m &rest others))
(define-rest-alias mmin (m &rest others))
(define-rest-alias mmax (m &rest others))

(define-simple-alias mrand (m var))



;; [ ] miref
;; [ ] mcref
;; [ ] msetf
;; [ ] meye
;; [ ] mrand
;; [ ] mcol
;; [ ] mrow
;; [ ] m=
;; [ ] m~=
;; [ ] m/=
;; [ ] m<
;; [ ] m>
;; [ ] m<=
;; [ ] m>=
;; [ ] m+
;; [ ] nm+
;; [ ] m-
;; [ ] nm-
;; [ ] m*
;; [ ] nm*
;; [ ] n*m
;; [ ] m/
;; [ ] nm/
;; [ ] mapply
;; [ ] mapplyf
;; [ ] mdet
;; [ ] minv
;; [ ] mtranspose
;; [ ] nmtranspose
;; [ ] mtrace
;; [ ] mminor
;; [ ] mcofactor
;; [ ] mcof
;; [ ] madj
;; [ ] mpivot
;; [ ] mlu
;; [ ] mtranslation
;; [ ] mscaling
;; [ ] mrotation
;; [ ] mlookat
;; [ ] mfrustum
;; [ ] mortho
;; [ ] mperspective
;; [ ] nmtranslate
;; [ ] nmscale
;; [ ] nmrotate
;; [ ] m1norm
;; [ ] minorm
;; [ ] m2norm
;; [ ] mqr
;; [ ] meigen
;; [ ] nmswap-row
;; [ ] nmswap-col
;; [ ] mdiag
;; [ ] mblock
;; [ ] mtop
;; [ ] mbottom
;; [ ] mleft
;; [ ] mright
