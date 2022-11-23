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
         :alias (list 0 'arr))
  (cond ((eql <s> 'n)
         (field (compose-name NIL (type-prefix <t>) 'mcols <s>) :type 'mat-dim :alias '(1 cols))
         (field (compose-name NIL (type-prefix <t>) 'mrows <s>) :type 'mat-dim :alias '(2 rows)))
        (T
         (field (compose-name NIL (type-prefix <t>) 'mcols <s>) :type `(eql ,<s>) :alias '(1 cols) :value <s>)
         (field (compose-name NIL (type-prefix <t>) 'mrows <s>) :type `(eql ,<s>) :alias '(2 rows) :value <s>))))

(defun attribute (type attribute &optional (mat-arg 'm))
  (destructuring-bind (<s> <t>) (template-arguments type)
    (declare (ignore <t>))
    (ecase attribute
      (:dim-type (if (eql 'n <s>) 'mat-dim `(integer 0 ,(1- <s>))))
      (:idx-type (if (eql 'n <s>) 'mat-dim `(integer 0 ,(1- (* <s> <s>)))))
      (:cols (if (eql 'n <s>) `(,(place type 'cols) ,mat-arg) <s>))
      (:rows (if (eql 'n <s>) `(,(place type 'rows) ,mat-arg) <s>))
      (:len  (if (eql 'n <s>) `(length (,(place type 'arr) ,mat-arg)) (* <s> <s>)))
      (:array `(,(place type 'arr) ,mat-arg)))))

(defmacro do-mat-combinations (template &rest other-template-args)
  `(do-combinations ,template ,@other-template-args (2 3 4 n)
     (#-3d-vectors-no-f32 f32
      #-3d-vectors-no-f64 f64
      #-3d-vectors-no-u32 u32
      #-3d-vectors-no-i32 i32)))

(do-mat-combinations define-mat)

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

(define-alias mat-p (thing)
  `(typep ,thing '*mat))

(defmacro define-mat-accessor (name slot)
  (let ((instances (instances 'mat-type)))
    `(progn
       (define-type-dispatch ,name (mat)
         ,@(loop for type in instances
                 collect `((,(lisp-type type)) ,(place-type type slot)
                           ,(place-form type slot 'mat))))
       (define-type-dispatch (setf ,name) (value mat)
         ,@(loop for type in instances
                 unless (read-only (slot type slot))
                 collect `((,(place-type type slot) ,(lisp-type type)) ,(place-type type slot)
                           (setf ,(place-form type slot 'mat) value)))))))

(define-mat-accessor marr arr)
(define-mat-accessor mcols cols)
(define-mat-accessor mrows rows)

;; [x] matX
;; [x] marrX
;; [x] matX-copy (autogen is not useful as it does not deep-copy the array)
;; [x] matX-p
;; [x] mirefX
;; [x] mcrefX
;; [x] mat (type)
;; [x] mat-p
;; [x] marr
;; [x] mcols
;; [x] mrows
;; [ ] mat
;; [ ] matf
;; [x] write-matrix
;; [ ] with-matrix
;; [ ] with-fast-matref
;; [ ] with-fast-matrefs
;; [ ] with-fast-matcase

(defun write-matrix (m stream &key (format :nice))
  (etypecase stream
    (null (with-output-to-string (out)
            (write-matrix m out :format format)))
    ((eql T) (write-matrix m *standard-output* :format format))
    (stream
     (ecase format
       (:nice
        (dotimes (i (mrows m))
          (cond ((or (= (mrows m) 1) (< 0 i (1- (mrows m))))
                 (write-string "│ " stream))
                ((= i 0)
                 (write-string "┌ " stream))
                (T
                 (write-string "└ " stream)))
          (dotimes (j (mcols m))
            (format stream "~10,3@e " (mcref m i j)))
          (cond ((or (= (mrows m) 1) (< 0 i (1- (mrows m))))
                 (write-string "│" stream))
                ((= i 0)
                 (write-string "┐" stream))
                (T
                 (write-string "┘" stream)))
          (unless (= i (1- (mrows m)))
            (terpri stream))))
       (:wolfram
        (write-string "{" stream)
        (dotimes (i (mrows m))
          (write-string "{" stream)
          (dotimes (j (mcols m))
            (format stream "~f~:[,~;~]" (mcref m i j) (= j (1- (mcols m)))))
          (write-string "}" stream)
          (unless (= i (1- (mrows m)))
            (write-string "," stream)))
        (write-string "}" stream))
       (:array
        (write-string "#2A(" stream)
        (dotimes (i (mrows m))
          (write-string "(" stream)
          (dotimes (j (mcols m))
            (format stream "~f~:[ ~;~]" (mcref m i j) (= j (1- (mcols m)))))
          (write-string ")" stream)
          (unless (= i (1- (mrows m)))
            (write-string " " stream)))
        (write-string ")" stream)))
     m)))
