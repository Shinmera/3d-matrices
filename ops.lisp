#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.matrix)

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
       (dbg "Expanding compiler macro (~a~{ ~a~})" ',name (list* value values))
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
       (dbg "Expanding compiler macro (~a~{ ~a~})" ',name (list* value values))
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
     `(,',func (mzero ,,(first args)) ,,@(lambda-list-variables args))))

(defmacro define-modifying-alias (name args &optional (func (compose-name NIL '! name)))
  `(define-alias ,name ,args
     `(,',func ,,(first args) ,,@(lambda-list-variables args))))

(defmacro define-simple-alias (name args &optional (func (compose-name NIL '! name)))
  `(progn (define-pure-alias ,name ,args ,func)
          (define-modifying-alias ,(compose-name NIL 'n name) ,args ,func)))

(defmacro define-rest-alias (name args &optional (func (compose-name NIL '! name)))
  (let ((vars (lambda-list-variables args))
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

;; [ ] mcopy
;; [ ] mzero
;; [ ] miref
;; [ ] mcref
;; [ ] msetf
;; [ ] meye
;; [ ] mrand
;; [ ] muniform
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
;; [ ] nmlookat
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
