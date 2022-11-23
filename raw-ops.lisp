#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.matrices)

(define-template copy <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,(lisp-type type))
               inline)
      (let* ((orig ,(place-form type 'arr 'm))
             (n ,(attribute type :len))
             (arr (make-array n :element-type ',<t>)))
        (dotimes (i n (,(constructor type) arr ,@(when (eql 'n <s>) `(,(place-form type 'cols 'm)
                                                                      ,(place-form type 'rows 'm)))))
          (setf (aref arr i) (aref orig i)))))))

(define-template zero <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,(lisp-type type))
               inline
               (ignorable m))
      (let* ((n ,(attribute type :len))
             (arr (make-array n :element-type ',<t>)))
        (dotimes (i n (,(constructor type) arr ,@(when (eql 'n <s>) `(,(place-form type 'cols 'm)
                                                                      ,(place-form type 'rows 'm)))))
          (setf (aref arr i) (load-time-value (,<t> 0))))))))

(define-template aref <s> <t> (m i)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(attribute type :idx-type) i)
               (return-type ,<t>)
               inline)
      (aref ,(place-form type 'arr 'm) i))))

(define-template cref <s> <t> (m y x)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(attribute type :dim-type) y x)
               (return-type ,<t>)
               inline)
      (aref ,(place-form type 'arr 'm)
            (+ (* y ,(place-form type 'cols 'm)) x)))))

(do-mat-combinations define-copy)
(do-mat-combinations define-zero)
(do-mat-combinations define-aref)
(do-mat-combinations define-cref)
