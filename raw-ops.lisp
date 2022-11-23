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
      (let ((orig ,(place-form type 'arr 'm))
            (arr (make-array ,(attribute type :len) :element-type ',<t>)))
        (do-times (i 0 ,(attribute type :len))
          (setf (aref arr i) (aref orig i)))
        (,(constructor type) arr ,@(when (eql 'n <s>) `(,(place-form type 'cols 'm)
                                                        ,(place-form type 'rows 'm))))))))

(define-template zero <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,(lisp-type type))
               inline
               (ignorable m))
      (let ((arr (make-array ,(attribute type :len) :element-type ',<t>)))
        (do-times (i 0 ,(attribute type :len))
          (setf (aref arr i) (load-time-value (,<t> 0))))
        (,(constructor type) arr ,@(when (eql 'n <s>) `(,(place-form type 'cols 'm)
                                                        ,(place-form type 'rows 'm))))))))

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

(define-template smatop <op> <st> <s> <t> (x m s)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x))
            (s (,<t> s)))
        (do-times (i 0 ,(attribute type :len) x)
          (setf (aref xa i) (,<op> (aref ma i) s)))))))

(define-template 2matop <op> <s> <t> (x m n)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m n)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (na ,(place-form type 'arr 'n))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len) x)
          (setf (aref xa i) (,<op> (aref ma i) (aref na i))))))))

(define-template 1matop <op> <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len) x)
          (setf (aref xa i) (,<op> (aref ma i))))))))

(define-template 2matreduce <red> <comb> rtype <s> <t> (m n)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m n)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (na ,(place-form type 'arr 'n))
             (r (,<comb> (aref ma 0) (aref na 0))))
        (do-times (i 1 ,(attribute type :len) r)
          (setf r (,<red> r (,<comb> (aref ma i) (aref na i)))))))))

(define-template 1matreduce <red> <comb> rtype <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (r (,<comb> (aref ma 0))))
        (do-times (i 1 ,(attribute type :len) r)
          (setf r (,<red> r (,<comb> (aref ma i)))))))))

(define-template smatreduce <red> <comb> <st> rtype <s> <t> (m s)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rtype (case rtype
                 (<t> <t>)
                 (float (case <t> (f64 'f64) (T 'f32)))
                 (T rtype))))
    `((declare (type ,(lisp-type type) m)
               (type ,(case <st> (<t> <t>) (T <st>)) s)
               (return-type ,rtype)
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (s (,<t> s))
             (r (,<comb> (aref ma 0) s)))
        (do-times (i 1 ,(attribute type :len))
          (setf r (,<red> r (,<comb> (aref ma i) s))))
        (,(if (member rtype '(f32 f64 i32 u32)) rtype 'progn) r)))))

;; [x] mcopy
;; [x] mzero
;; [x] miref
;; [x] mcref
;; [ ] msetf
;; [ ] meye
;; [ ] mrand
;; [ ] muniform
;; [ ] mcol
;; [ ] mrow
;; [x] m= m~= m/= m< m> m<= m>=
;; [x] m+ m-
;; [ ] m* m/
;; [ ] mapply
;; [ ] mapplyf
;; [ ] mdet
;; [ ] minv
;; [ ] mtranspose
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
;; [ ] m1norm
;; [ ] minorm
;; [ ] m2norm
;; [ ] mqr
;; [ ] meigen
;; [ ] mswap-row
;; [ ] mswap-col
;; [ ] mdiag
;; [ ] mblock
;; [ ] mtop
;; [ ] mbottom
;; [ ] mleft
;; [ ] mright

(do-mat-combinations define-copy)
(do-mat-combinations define-zero)
(do-mat-combinations define-aref)
(do-mat-combinations define-cref)
(do-mat-combinations define-smatop (+ - * / min max) (<t> real))
(do-mat-combinations define-1matop (- / identity))
(do-mat-combinations define-2matreduce (and) (= ~= /= < <= >= >) boolean)
(do-mat-combinations define-smatreduce (and) (= ~= /= < <= >= >) (<t> real) boolean)
(do-mat-combinations define-2matreduce (or) (/=) boolean)
(do-mat-combinations define-smatreduce (or) (/=) (<t> real) boolean)
