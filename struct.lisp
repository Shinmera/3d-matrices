#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

(defmacro %mka (&environment env size &key element contents)
  `(make-array ,size :element-type ',*float-type*
               ,@(if element
                     `(:initial-element ,(ensure-float-param element env))
                     `(:initial-contents ,contents))))

(defmacro %proper-array (size elements)
  `(etypecase ,elements
     (null (%mka ,size :element ,0))
     (real (%mka ,size :element ,elements))
     (sequence (let ((array (%mka ,size :element 0)))
                 (map-into array #'ensure-float ,elements)))))

(defun %proper-array-form (size elements)
  (let ((el (gensym "ELEMENTS")))
    (cond ((or (typep elements 'vector)
               (and (typep elements 'list)
                    (eql (first elements) 'quote)
                    (listp (second elements))))
           `(%mka ,size :contents (load-time-value
                                   (map 'list #'ensure-float ,elements))))
          ((null elements)
           `(%mka ,size :element 0))
          ((numberp size)
           `(%mka ,size :contents (load-time-value
                                   (let ((,el ,elements))
                                     (etypecase ,el
                                       (null (make-list ,size :initial-element (ensure-float 0)))
                                       (real (make-list ,size :initial-element (ensure-float ,el)))
                                       (sequence (map 'list #'ensure-float ,el)))))))
          ;; There's other cases but we can't really catch them because typep is not
          ;; actually useful. Ah well, this will have to be good enough for most.
          (T
           `(let ((,el ,elements))
              (etypecase ,el
                (real (%mka ,size :element ,el))
                (sequence (let ((array (%mka ,size :element 0)))
                            (map-into array #'ensure-float ,el)))))))))

(defstruct (mat2 (:conc-name NIL)
                   (:constructor %mat2 (%marr2))
                   (:copier mcopy2)
                   (:predicate mat2-p))
  (%marr2 NIL :type (simple-array #.*float-type* (4))))

(declaim (inline miref2))
(declaim (ftype (function (mat2 (integer 0 3)) #.*float-type*) miref2))
(define-ofun miref2 (mat i)
  (aref (%marr2 mat) i))

(defsetf miref2 (&environment env mat i) (value)
  `(setf (aref (%marr2 ,mat) ,i) ,(ensure-float-param value env)))

(declaim (inline mcref2))
(declaim (ftype (function (mat2 (integer 0 1) (integer 0 1)) #.*float-type*) mcref2))
(define-ofun mcref2 (mat y x)
  (aref (%marr2 mat) (+ (* y 2) x)))

(defsetf mcref2 (&environment env mat y x) (value)
  `(setf (aref (%marr2 ,mat) (+ (* ,y 2) ,x)) ,(ensure-float-param value env)))

(define-ofun mat2 (&optional elements)
  (%mat2 (%proper-array 4 elements)))

(define-compiler-macro mat2 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat2 ,(%proper-array-form 4 elements)))
        (T whole)))

(defmethod print-object ((m mat2) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat2) &optional env)
  (declare (ignore env))
  `(mat2 ,(%marr2 m)))

(defstruct (mat3 (:conc-name NIL)
                   (:constructor %mat3 (%marr3))
                   (:copier mcopy3)
                   (:predicate mat3-p))
  (%marr3 NIL :type (simple-array #.*float-type* (9))))

(declaim (inline miref3))
(declaim (ftype (function (mat3 (integer 0 8)) #.*float-type*) miref3))
(define-ofun miref3 (mat i)
  (aref (%marr3 mat) i))

(defsetf miref3 (&environment env mat i) (value)
  `(setf (aref (%marr3 ,mat) ,i) ,(ensure-float-param value env)))

(declaim (inline mcref3))
(declaim (ftype (function (mat3 (integer 0 2) (integer 0 2)) #.*float-type*) mcref3))
(define-ofun mcref3 (mat x y)
  (aref (%marr3 mat) (+ (* y 3) x)))

(defsetf mcref3 (&environment env mat y x) (value)
  `(setf (aref (%marr3 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

(define-ofun mat3 (&optional elements)
  (%mat3 (%proper-array 9 elements)))

(define-compiler-macro mat3 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat3 ,(%proper-array-form 9 elements)))
        (T whole)))

(defmethod print-object ((m mat3) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat3) &optional env)
  (declare (ignore env))
  `(mat3 ,(%marr3 m)))

(defstruct (mat4 (:conc-name NIL)
                   (:constructor %mat4 (%marr4))
                   (:copier mcopy4)
                   (:predicate mat4-p))
  (%marr4 NIL :type (simple-array #.*float-type* (16))))

(declaim (inline miref4))
(declaim (ftype (function (mat4 (integer 0 15)) #.*float-type*) miref4))
(define-ofun miref4 (mat i)
  (aref (%marr4 mat) i))

(defsetf miref4 (&environment env mat i) (value)
  `(setf (aref (%marr4 ,mat) ,i) ,(ensure-float-param value env)))

(declaim (inline mcref4))
(declaim (ftype (function (mat4 (integer 0 3) (integer 0 3)) #.*float-type*) mcref4))
(define-ofun mcref4 (mat y x)
  (aref (%marr4 mat) (+ (* y 3) x)))

(defsetf mcref4 (&environment env mat y x) (value)
  `(setf (aref (%marr4 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

(define-ofun mat4 (&optional elements)
  (%mat4 (%proper-array 16 elements)))

(define-compiler-macro mat4 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat4 ,(%proper-array-form 16 elements)))
        (T whole)))

(defmethod print-object ((m mat4) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat4) &optional env)
  (declare (ignore env))
  `(mat4 ,(%marr4 m)))

(defstruct (matn (:conc-name NIL)
                 (:constructor %matn (%rows %cols %marrn))
                 (:copier mcopyn)
                 (:predicate matn-p))
  (%rows NIL :type mat-dim)
  (%cols NIL :type mat-dim)
  (%marrn NIL :type (simple-array #.*float-type*)))

(declaim (inline mirefn))
(declaim (ftype (function (matn (integer 0 #.(expt *matrix-limit* 2))) #.*float-type*) mirefn))
(define-ofun mirefn (mat i)
  (aref (%marrn mat) i))

(defsetf mirefn (&environment env mat i) (value)
  `(setf (aref (%marrn ,mat) ,i) ,(ensure-float-param value env)))

(declaim (inline mcrefn))
(declaim (ftype (function (matn mat-dim mat-dim) #.*float-type*) mcrefn))
(define-ofun mcrefn (mat y x)
  (aref (%marrn mat) (+ (* y (%cols mat)) x)))

(defsetf mcrefn (&environment env mat y x) (value)
  `(setf (aref (%marrn ,mat) (+ (* ,y (%cols ,mat)) ,x)) ,(ensure-float-param value env)))

(define-ofun matn (r c &optional elements)
  (check-type r mat-dim)
  (check-type c mat-dim)
  (%matn c r (%proper-array (* r c) elements)))

(define-compiler-macro matn (&whole whole &environment env r c &optional elements)
  (cond ((constantp elements env)
         `(%matn ,r ,c ,(%proper-array-form `(* ,c ,r) elements)))
        (T whole)))

(defmethod print-object ((m matn) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m matn) &optional env)
  (declare (ignore env))
  `(matn ,(%rows m) ,(%cols m) ,(%marrn m)))

;; Compat
(deftype mat ()
  `(or mat2 mat3 mat4 matn))

(declaim (inline marr))
(declaim (ftype (function (mat) (simple-array #.*float-type*)) marr))
(defun marr (mat)
  (etypecase mat
    (mat2 (%marr2 mat))
    (mat3 (%marr3 mat))
    (mat4 (%marr4 mat))
    (matn (%marrn mat))))

(declaim (inline miref))
(declaim (ftype (function (mat (integer 0 #.(expt *matrix-limit* 2))) #.*float-type*) miref))
(defun miref (mat i)
  (etypecase mat
    (mat2 (miref2 mat i))
    (mat3 (miref3 mat i))
    (mat4 (miref4 mat i))
    (matn (mirefn mat i))))

(declaim (inline mcref))
(declaim (ftype (function (mat mat-dim mat-dim) #.*float-type*) mcref))
(defun mcref (mat y x)
  (etypecase mat
    (mat2 (mcref2 mat y x))
    (mat3 (mcref3 mat y x))
    (mat4 (mcref4 mat y x))
    (matn (mcrefn mat y x))))

(declaim (inline mcols))
(declaim (ftype (function (mat) mat-dim) mcols))
(defun mcols (mat)
  (etypecase mat
    (mat2 2)
    (mat3 3)
    (mat4 4)
    (matn (%cols mat))))

(declaim (inline mrows))
(declaim (ftype (function (mat) mat-dim) mrows))
(defun mrows (mat)
  (etypecase mat
    (mat2 2)
    (mat3 3)
    (mat4 4)
    (matn (%rows mat))))

(defun mat (&rest vals)
  (let ((len (length vals)))
    (case len
      (4 (mat2 vals))
      (9 (mat3 vals))
      (16 (mat4 vals))
      (T (let* ((sqrt (sqrt len))
                (rows (floor sqrt)))
           (unless (= rows sqrt)
             (error "Number of values ~a is not square-- don't know how to turn into matrix. Please use MATN and specify the rows and columns explicitly."
                    len))
           (matn rows rows vals))))))

(define-compiler-macro mat (&whole whole &environment env &rest vals)
  (let ((len (length vals))
        (m (gensym "M")))
    (case len
      (4 `(let ((,m (mat2)))
            ,@(loop for i from 0 below 4 for v in vals
                    collect `(setf (aref (%marr2 ,m) ,i) ,(ensure-float-param v env)))
            ,m))
      (9 `(let ((,m (mat3)))
            ,@(loop for i from 0 below 9 for v in vals
                    collect `(setf (aref (%marr3 ,m) ,i) ,(ensure-float-param v env)))
            ,m))
      (16 `(let ((,m (mat4)))
             ,@(loop for i from 0 below 16 for v in vals
                     collect `(setf (aref (%marr4 ,m) ,i) ,(ensure-float-param v env)))
             ,m))
      (T whole))))

(defun matf (mat &rest vals)
  (map-into (marr mat) #'ensure-float vals)
  mat)

(define-compiler-macro matf (&whole whole &environment env mat &rest vals)
  (let ((len (length vals))
        (m (gensym "M")))
    (case len
      (4 `(let ((,m ,mat))
            ,@(loop for i from 0 below 4 for v in vals
                    collect `(setf (aref (%marr2 ,m) ,i) ,(ensure-float-param v env)))
            ,m))
      (9 `(let ((,m ,mat))
            ,@(loop for i from 0 below 9 for v in vals
                    collect `(setf (aref (%marr3 ,m) ,i) ,(ensure-float-param v env)))
            ,m))
      (16 `(let ((,m ,mat))
             ,@(loop for i from 0 below 16 for v in vals
                     collect `(setf (aref (%marr4 ,m) ,i) ,(ensure-float-param v env)))
             ,m))
      (T whole))))
