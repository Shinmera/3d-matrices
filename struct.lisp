#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

(defmacro %proper-array (size elements)
  `(etypecase ,elements
     (null (make-array ,size :element-type ',*float-type* :initial-element ,(ensure-float 0)))
     (real (make-array ,size :element-type ',*float-type* :initial-element (ensure-float ,elements)))
     (sequence (let ((array (make-array ,size :element-type ',*float-type* :initial-element #.(ensure-float 0))))
                 (map-into array #'ensure-float ,elements)
                 array))))

(defun %proper-array-form (size elements)
  (let ((el (gensym "ELEMENTS")))
    `(%mat2x2 (make-array ,size :element-type ',*float-type*
                                :initial-element ,(ensure-float 0)
                                :initial-elements (load-time-value
                                                   (let ((,el ,elements))
                                                     (etypecase ,el
                                                       (null (ensure-float 0))
                                                       (real (ensure-float ,el))
                                                       (sequence (map 'list #'ensure-float ,el)))))))))

(defstruct (mat2x2 (:conc-name NIL)
                   (:constructor %mat2x2 (%e2x2))
                   (:copier mcopy2x2)
                   (:predicate mat2x2-p))
  (%e2x2 NIL :type (simple-array #.*float-type* (4))))

(declaim (inline e2x2))
(declaim (ftype (function (mat2x2 (integer 0 1) (integer 0 1)) #.*float-type*) e2x2))
(define-ofun e2x2 (mat x y)
  (aref (%e2x2 mat) (+ (* y 2) x)))

(defsetf e2x2 (&environment env mat x y) (value)
  `(setf (aref (%e2x2 ,mat) (+ (* ,y 2) ,x)) ,(ensure-float-param value env)))

(define-ofun mat2x2 (&optional elements)
  (%mat2x2 (%proper-array 4 elements)))

(define-compiler-macro mat2x2 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         (%proper-array-form 4 elements))
        (T whole)))

(defmethod print-object ((m mat2x2) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat2x2) &optional env)
  (declare (ignore env))
  `(mat2x2 ,(%e2x2 m)))

(defstruct (mat3x3 (:conc-name NIL)
                   (:constructor %mat3x3 (%e3x3))
                   (:copier mcopy3x3)
                   (:predicate mat3x3-p))
  (%e3x3 NIL :type (simple-array #.*float-type* (9))))

(declaim (inline e3x3))
(declaim (ftype (function (mat3x3 (integer 0 2) (integer 0 2)) #.*float-type*) e3x3))
(define-ofun e3x3 (mat x y)
  (aref (%e3x3 mat) (+ (* y 3) x)))

(defsetf e3x3 (&environment env mat x y) (value)
  `(setf (aref (%e3x3 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

(define-ofun mat3x3 (&optional elements)
  (%mat3x3 (%proper-array 9 elements)))

(define-compiler-macro mat3x3 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         (%proper-array-form 9 elements))
        (T whole)))

(defmethod print-object ((m mat3x3) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat3x3) &optional env)
  (declare (ignore env))
  `(mat3x3 ,(%e3x3 m)))

(defstruct (mat4x4 (:conc-name NIL)
                   (:constructor %mat4x4 (%e4x4))
                   (:copier mcopy4x4)
                   (:predicate mat4x4-p))
  (%e4x4 NIL :type (simple-array #.*float-type* (16))))

(declaim (inline e4x4))
(declaim (ftype (function (mat4x4 (integer 0 3) (integer 0 3)) #.*float-type*) e4x4))
(define-ofun e4x4 (mat x y)
  (aref (%e4x4 mat) (+ (* y 3) x)))

(defsetf e4x4 (&environment env mat x y) (value)
  `(setf (aref (%e4x4 ,mat) (+ (* ,y 3) ,x)) ,(ensure-float-param value env)))

(define-ofun mat4x4 (&optional elements)
  (%mat4x4 (%proper-array 16 elements)))

(define-compiler-macro mat4x4 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         (%proper-array-form 16 elements))
        (T whole)))

(defmethod print-object ((m mat4x4) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat4x4) &optional env)
  (declare (ignore env))
  `(mat4x4 ,(%e4x4 m)))

(defstruct (matnxn (:conc-name NIL)
                   (:constructor %matnxn (%cols %rows %enxn))
                   (:copier mcopynxn)
                   (:predicate matnxn-p))
  (%cols NIL :type (integer 1 #.*matrix-limit*))
  (%rows NIL :type (integer 1 #.*matrix-limit*))
  (%enxn NIL :type (simple-array #.*float-type*)))

(declaim (inline enxn))
(declaim (ftype (function (matnxn (integer 0 #.(1- *matrix-limit*))
                                  (integer 0 #.(1- *matrix-limit*))) #.*float-type*) enxn))
(define-ofun enxn (mat x y)
  (aref (%enxn mat) (+ (* y (%cols mat)) x)))

(defsetf enxn (&environment env mat x y) (value)
  `(setf (aref (%enxn ,mat) (+ (* ,y (%cols ,mat)) ,x)) ,(ensure-float-param value env)))

(declaim (inline cols))
(declaim (ftype (function (matnxn) (integer 0 #.*matrix-limit*)) cols))
(defun cols (m) (%cols m))

(declaim (inline rows))
(declaim (ftype (function (matnxn) (integer 0 #.*matrix-limit*)) rows))
(defun rows (m) (%rows m))

(define-ofun matnxn (c r &optional elements)
  (check-type c (integer 1 #.*matrix-limit*))
  (check-type r (integer 1 #.*matrix-limit*))
  (%matnxn c r (%proper-array (* c r) elements)))

(define-compiler-macro matnxn (&whole whole &environment env c r &optional elements)
  (cond ((constantp elements env)
         (%proper-array-form `(* ,c ,r) elements))
        (T whole)))

(defmethod print-object ((m matnxn) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m matnxn) &optional env)
  (declare (ignore env))
  `(matnxn ,(%cols m) ,(%rows m) ,(%enxn m)))
