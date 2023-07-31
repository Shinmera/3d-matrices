#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.matrices)

(deftype dimension ()
  `(integer 1 ,(truncate (sqrt array-dimension-limit))))

(define-template-type mat (<s> <t>)
    (compose-name NIL (type-prefix <t>) 'mat <s>)
  :make-object (case <s>
                 (n (list (compose-name NIL (type-prefix <t>) 'mat <s>)
                          '(n m &rest arr)))
                 (T (list (compose-name NIL (type-prefix <t>) 'mat <s>)
                          '(&rest arr))))
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
(deftype mat () 'fmat)

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

(defmacro define-mat-constructor (size type)
  (let ((name (compose-name NIL (type-prefix type) 'mat size))
        (lisp-type (lisp-type (type-instance 'mat-type size type))))
    (etypecase size
      ((eql n)
       `(progn
          (export '(,name))
          (define-type-dispatch ,name (n m &rest args)
            ((dimension dimension) ,lisp-type
             (,(constructor (type-instance 'mat-type size type))
              (map-into (make-array (* n m) :element-type ',type :initial-element (,type 0))
                        #',type args)
              n m)))))
      (integer
       (let ((vec-type (type-instance 'vec-type size type))
             (args (loop for i from 0 below (* size size) collect (compose-name NIL 'v i))))
         (flet ((constructor (&rest args)
                  `(,(constructor (type-instance 'mat-type size type))
                     ,(if (rest args)
                          `(v::%vec-array ,(length args) ,type ,@args)
                          `(map-into (make-array ,(* size size) :element-type ',type)
                                     #',type ,(first args))))))
           `(progn
              (export '(,name))
              (define-type-dispatch ,name (&optional ,@args)
                (,(loop repeat (length args) collect 'null) ,lisp-type
                 ,(apply #'constructor (make-list (length args) :initial-element 0)))
                (,(loop repeat (length args) collect 'real) ,lisp-type
                 ,(apply #'constructor args))
                ((real ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(apply #'constructor (make-list (length args) :initial-element (first args))))
                ((,lisp-type ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 (,(compose-name NIL (type-prefix type) 'mat size '-copy) ,(first args)))
                ((,(compose-name NIL '* 'mat size) ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(constructor `(marr ,(first args))))
                ((vector ,@(loop repeat (1- (length args)) collect 'null)) ,lisp-type
                 ,(constructor (first args)))
                ((,@(loop repeat size collect (lisp-type vec-type))
                  ,@(loop repeat (- (length args) size) collect 'null)) ,lisp-type
                 ,(apply #'constructor (loop for i from 0 below size
                                             append (loop for arg in args repeat size
                                                          collect (place-form vec-type i arg)))))))))))))

(defmacro define-mat*-constructor (type)
  (let ((name (compose-name NIL (type-prefix type) 'mat))
        (args (loop for i from 0 below (* 4 4) collect (compose-name NIL 'v i))))
    (labels ((make (size args)
               `(,(constructor (type-instance 'mat-type size type))
                  ,(if (rest args)
                       `(v::%vec-array ,(length args) ,type ,@args)
                       `(map-into (make-array ,(* size size) :element-type ',type)
                                  #',type ,(first args)))))
             (args (n)
               (append (loop repeat (* n n) collect 'real)
                       (loop repeat (- (* 4 4) (* n n)) collect 'null)))
             (make-vector (n)
               `(,(constructor (type-instance 'mat-type n type))
                  (map-into (make-array ,(* n n) :element-type ',type)
                            #',type ,(first args))))
             (case-vec (n)
               (let ((vec-type (type-instance 'vec-type n type)))
                 `((,@(loop repeat n collect (lisp-type vec-type))
                    ,@(loop repeat (- (length args) n) collect 'null))
                   ,(lisp-type (type-instance 'mat-type n type))
                   ,(make n (loop for i from 0 below n
                                  append (loop for arg in args repeat n
                                               collect (place-form vec-type i arg))))))))
      `(progn
         (export '(,name))
         (define-type-dispatch ,name (&optional ,@args)
           (,(args 2) ,(lisp-type (type-instance 'mat-type 2 type))
            ,(make 2 (subseq args 0 4)))
           (,(args 3) ,(lisp-type (type-instance 'mat-type 3 type))
            ,(make 3 (subseq args 0 9)))
           (,(args 4) ,(lisp-type (type-instance 'mat-type 4 type))
            ,(make 4 args))
           ((mat ,@(loop repeat 15 collect 'null)) *mat
            (mcopy ,(first args)))
           ((vector ,@(loop repeat 15 collect 'null)) ,(ecase type
                                                         (f32 'mat)
                                                         (f64 'dmat)
                                                         (i32 'imat)
                                                         (u32 'umat))
            (ecase (length ,(first args))
              (2 ,(make-vector 2))
              (3 ,(make-vector 3))
              (4 ,(make-vector 4))))
           ,(case-vec 2)
           ,(case-vec 3)
           ,(case-vec 4))))))

(do-mat-combinations define-mat-constructor)
(do-combinations define-mat*-constructor
  (#-3d-vectors-no-f32 f32
   #-3d-vectors-no-f64 f64
   #-3d-vectors-no-u32 u32
   #-3d-vectors-no-i32 i32))

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
        (write-string ")" stream))
       (:c
        (write-string "{")
        (dotimes (i (mrows m))
          (dotimes (j (mcols m))
            (format stream "~f~:[, ~;~]" (mcref m i j) (= j (1- (mcols m)))))
          (unless (= i (1- (mrows m)))
            (format stream ",~% ")))
        (write-string "}")))
     m)))
