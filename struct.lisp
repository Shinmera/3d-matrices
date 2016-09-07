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
     (null `(%mka ,size :element ,0))
     (real `(%mka ,size :element ,elements))
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
                   (:constructor %mat2 (%me2))
                   (:copier mcopy2)
                   (:predicate mat2-p))
  (%me2 NIL :type (simple-array #.*float-type* (4))))

(declaim (inline me2))
(declaim (ftype (function (mat2 (integer 0 1) (integer 0 1)) #.*float-type*) me2))
(define-ofun me2 (mat x y)
  (aref (%me2 mat) (+ (* y 2) x)))

(defsetf me2 (&environment env mat x y) (value)
  `(setf (aref (%me2 ,mat) (+ (* ,y 2) ,x)) ,(ensure-float-param value env)))

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
  `(mat2 ,(%me2 m)))

(defstruct (mat3 (:conc-name NIL)
                   (:constructor %mat3 (%me3))
                   (:copier mcopy3)
                   (:predicate mat3-p))
  (%me3 NIL :type (simple-array #.*float-type* (9))))

(declaim (inline me3))
(declaim (ftype (function (mat3 (integer 0 2) (integer 0 2)) #.*float-type*) me3))
(define-ofun me3 (mat x y)
  (aref (%me3 mat) (+ (* y 3) x)))

(defsetf me3 (&environment env mat x y) (value)
  `(setf (aref (%me3 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

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
  `(mat3 ,(%me3 m)))

(defstruct (mat4 (:conc-name NIL)
                   (:constructor %mat4 (%me4))
                   (:copier mcopy4)
                   (:predicate mat4-p))
  (%me4 NIL :type (simple-array #.*float-type* (16))))

(declaim (inline me4))
(declaim (ftype (function (mat4 (integer 0 3) (integer 0 3)) #.*float-type*) me4))
(define-ofun me4 (mat x y)
  (aref (%me4 mat) (+ (* y 3) x)))

(defsetf me4 (&environment env mat x y) (value)
  `(setf (aref (%me4 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

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
  `(mat4 ,(%me4 m)))

(defstruct (matn (:conc-name NIL)
                   (:constructor %matn (%cols %rows %men))
                   (:copier mcopyn)
                   (:predicate matn-p))
  (%cols NIL :type (integer 1 #.*matrix-limit*))
  (%rows NIL :type (integer 1 #.*matrix-limit*))
  (%men NIL :type (simple-array #.*float-type*)))

(declaim (inline men))
(declaim (ftype (function (matn mat-dim mat-dim) #.*float-type*) men))
(define-ofun men (mat x y)
  (aref (%men mat) (+ (* y (%cols mat)) x)))

(defsetf men (&environment env mat x y) (value)
  `(setf (aref (%men ,mat) (+ (* ,y (%cols ,mat)) ,x)) ,(ensure-float-param value env)))

(define-ofun matn (c r &optional elements)
  (check-type c (integer 1 #.*matrix-limit*))
  (check-type r (integer 1 #.*matrix-limit*))
  (%matn c r (%proper-array (* c r) elements)))

(define-compiler-macro matn (&whole whole &environment env c r &optional elements)
  (cond ((constantp elements env)
         `(%matn ,c ,r ,(%proper-array-form `(* ,c ,r) elements)))
        (T whole)))

(defmethod print-object ((m matn) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m matn) &optional env)
  (declare (ignore env))
  `(matn ,(%cols m) ,(%rows m) ,(%men m)))

;; Compat
(deftype mat ()
  `(or mat2 mat3 mat4 matn))

(declaim (inline me))
(declaim (ftype (function (mat (integer 0 #.*matrix-limit*) (integer 0 #.*matrix-limit*)) #.*float-type*) me))
(defun me (mat x y)
  (etypecase mat
    (mat2 (me2 mat x y))
    (mat3 (me3 mat x y))
    (mat4 (me4 mat x y))
    (matn (men mat x y))))

(declaim (inline cols))
(declaim (ftype (function (mat) (integer 0 #.*matrix-limit*)) cols))
(defun mcols (mat)
  (etypecase mat
    (mat2 2)
    (mat3 3)
    (mat4 4)
    (matn (%cols mat))))

(declaim (inline rows))
(declaim (ftype (function (mat) (integer 0 #.*matrix-limit*)) rows))
(defun mrows (mat)
  (etypecase mat
    (mat2 2)
    (mat3 3)
    (mat4 4)
    (matn (%rows mat))))
