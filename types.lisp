#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.matrices)

(define-template-type mat (<s> <t>)
  (compose-name NIL (type-prefix <t>) 'mat <s>)
  (field (compose-name NIL (type-prefix <t>) 'marr <s>)
         :type `(simple-array ,<t> (,(if (integerp <s>) (* <s> <s>) '*)))
         :alias (list 0 'arr)))

(defmacro do-mat-combinations (template &rest other-template-args)
  `(do-combinations ,template ,@other-template-args (2 3 4 n)
     (#-3d-vectors-no-f32 f32
      #-3d-vectors-no-f64 f64
      #-3d-vectors-no-u32 u32
      #-3d-vectors-no-i32 i32)))

(do-mat-combinations define-mat)

(defmacro define-mat-accessor (name i)
  (let ((instances (instances 'mat-type)))
    `(progn
       (define-type-dispatch ,name (mat)
         ,@(loop for type in instances
                 collect `((,(lisp-type type)) ,(place-type type 0)
                           (,(place type i) mat))))
       (define-type-dispatch (setf ,name) (value mat)
         ,@(loop for type in instances
                 collect `((,(place-type type 0) ,(lisp-type type)) ,(place-type type 0)
                           (setf (,(place type i) mat) value)))))))

(define-mat-accessor marr 0)

#-3d-vectors-no-f32 (define-type-alias fmat mat2 mat3 mat4 matn)
#-3d-vectors-no-f64 (define-type-alias dmat dmat2 dmat3 dmat4 dmatn)
#-3d-vectors-no-i32 (define-type-alias imat imat2 imat3 imat4 imatn)
#-3d-vectors-no-u32 (define-type-alias umat umat2 umat3 umat4 umatn)
(define-type-alias *mat2
  #-3d-vectors-no-f32 mat2 #-3d-vectors-no-f64 dmat2 #-3d-vectors-no-i32 imat2 #-3d-vectors-no-u32 umat2)
(define-type-alias *mat3
  #-3d-vectors-no-f32 mat3 #-3d-vectors-no-f64 dmat3 #-3d-vectors-no-i32 imat3 #-3d-vectors-no-u32 umat3)
(define-type-alias *mat4
  #-3d-vectors-no-f32 mat4 #-3d-vectors-no-f64 dmat4 #-3d-vectors-no-i32 imat4 #-3d-vectors-no-u32 umat4)
(define-type-alias *matn
  #-3d-vectors-no-f32 matn #-3d-vectors-no-f64 dmatn #-3d-vectors-no-i32 imatn #-3d-vectors-no-u32 umatn)
(define-type-alias *mat
  #-3d-vectors-no-f32 mat2 #-3d-vectors-no-f64 dmat2 #-3d-vectors-no-i32 imat2 #-3d-vectors-no-u32 umat2
  #-3d-vectors-no-f32 mat3 #-3d-vectors-no-f64 dmat3 #-3d-vectors-no-i32 imat3 #-3d-vectors-no-u32 umat3
  #-3d-vectors-no-f32 mat4 #-3d-vectors-no-f64 dmat4 #-3d-vectors-no-i32 imat4 #-3d-vectors-no-u32 umat4
  #-3d-vectors-no-f32 matn #-3d-vectors-no-f64 dmatn #-3d-vectors-no-i32 imatn #-3d-vectors-no-u32 umatn)
(deftype mat () '*mat)

(macrolet ((emit ()
             `(define-type-dispatch mcopy (a)
                ,@(loop for instance in (instances 'mat-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance) (,(compose-name #\- (lisp-type instance) 'copy) a))))))
  (emit))

(macrolet ((emit ()
             `(define-type-dispatch mzero (a)
                ,@(loop for instance in (instances 'mat-type)
                        collect `((,(lisp-type instance)) ,(lisp-type instance) (,(constructor instance) ,@(loop for place in (places instance)
                                                                                                                 collect `(,(third place) 0))))))))
  (emit))



;; [x] matX
;; [x] marrX
;; [x] matX-copy
;; [x] matX-p
;; [ ] mirefX
;; [ ] mcrefX
;; [ ] mat (type)
;; [ ] mat-p
;; [x] marr
;; [ ] miref
;; [ ] mcref
;; [ ] mcols
;; [ ] mrows
;; [ ] mat
;; [ ] matf
;; [ ] mcopy
;; [ ] write-matrix
;; [ ] with-matrix
;; [ ] with-fast-matref
;; [ ] with-fast-matrefs
;; [ ] with-fast-matcase

