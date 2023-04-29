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
        (do-times (i 0 ,(attribute type :len) 1 x)
          (setf (aref xa i) (,<op> (aref ma i) s)))))))

(define-template 2matop <op> <s> <t> (x m n)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m n)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (na ,(place-form type 'arr 'n))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len) 1 x)
          (setf (aref xa i) (,<op> (aref ma i) (aref na i))))))))

(define-template 1matop <op> <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len) 1 x)
          (setf (aref xa i) (,<op> (aref ma i))))))))

(define-template 0matop <op> <s> <t> (x)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x)
               (return-type ,(lisp-type type))
               inline)
      (let ((xa ,(place-form type 'arr 'x)))
        (do-times (i 0 ,(attribute type :len 'x) 1 x)
          (multiple-value-bind (y x) (floor i ,(attribute type :cols 'x))
            (setf (aref xa i) (,<t> (,<op> ',<t> x y)))))))))

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
        (do-times (i 1 ,(attribute type :len) 1 r)
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
        (do-times (i 1 ,(attribute type :len) 1 r)
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

(define-template m*m <s> <t> (x m n)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m n)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (na ,(place-form type 'arr 'n))
            (xa ,(place-form type 'arr 'x))
            (mc ,(attribute type :cols 'm))
            (nc ,(attribute type :cols 'n))
            (xi 0) (mi 0))
        (do-times (xy 0 ,(attribute type :rows 'x) 1 x)
          (do-times (xx 0 ,(attribute type :cols 'x) 1)
            (let ((c (,<t> 0))
                  (mi mi)
                  (ni xx))
              (do-times (i 0 ,(attribute type :cols 'm) 1)
                (setf c (,<t> (+ c (* (aref ma mi) (aref na ni)))))
                (incf ni nc)
                (incf mi 1))
              (setf (aref xa xi) c)
              (incf xi)))
          (incf mi mc))))))

(define-template mdet <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>)
               inline)
      (let ((ma ,(place-form type 'arr 'm)))
        ,(case <s>
           ;; FIXME: This *sucks*. Can't we compute this expansion?
           (2 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 2)))))
                 (- (* (e 0 0) (e 1 1))
                    (* (e 0 1) (e 1 0)))))
           (3 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 3)))))
                 (- (+ (* (e 0 0) (e 1 1) (e 2 2))
                       (* (e 0 1) (e 1 2) (e 2 0))
                       (* (e 0 2) (e 1 0) (e 2 1)))
                    (+ (* (e 0 0) (e 1 2) (e 2 1))
                       (* (e 0 1) (e 1 0) (e 2 2))
                       (* (e 0 2) (e 1 1) (e 2 0))))))
           (4 `(macrolet ((e (y x) `(aref ma (+ ,x (* ,y 4)))))
                 (- (+ (* (e 0 3) (e 1 2) (e 2 1) (e 3 0)) (* (e 0 0) (e 1 1) (e 2 2) (e 3 3))
                       (* (e 0 1) (e 1 3) (e 2 2) (e 3 0)) (* (e 0 2) (e 1 1) (e 2 3) (e 3 0))
                       (* (e 0 2) (e 1 3) (e 2 0) (e 3 1)) (* (e 0 3) (e 1 0) (e 2 2) (e 3 1))
                       (* (e 0 0) (e 1 2) (e 2 3) (e 3 1)) (* (e 0 3) (e 1 1) (e 2 0) (e 3 2))
                       (* (e 0 0) (e 1 3) (e 2 1) (e 3 2)) (* (e 0 1) (e 1 0) (e 2 3) (e 3 2))
                       (* (e 0 1) (e 1 2) (e 2 0) (e 3 3)) (* (e 0 2) (e 1 0) (e 2 1) (e 3 3)))
                    (+ (* (e 0 2) (e 1 3) (e 2 1) (e 3 0)) (* (e 0 3) (e 1 1) (e 2 2) (e 3 0))
                       (* (e 0 1) (e 1 2) (e 2 3) (e 3 0)) (* (e 0 3) (e 1 2) (e 2 0) (e 3 1))
                       (* (e 0 0) (e 1 3) (e 2 2) (e 3 1)) (* (e 0 2) (e 1 0) (e 2 3) (e 3 1))
                       (* (e 0 1) (e 1 3) (e 2 0) (e 3 2)) (* (e 0 3) (e 1 0) (e 2 1) (e 3 2))
                       (* (e 0 0) (e 1 1) (e 2 3) (e 3 2)) (* (e 0 2) (e 1 1) (e 2 0) (e 3 3))
                       (* (e 0 0) (e 1 2) (e 2 1) (e 3 3)) (* (e 0 1) (e 1 0) (e 2 2) (e 3 3))))))
           (T
            `(multiple-value-bind (LU P s) (mlu m)
               (declare (ignore P))
               (let ((rows (mrows LU))
                     (cols (mcols LU))
                     (arr (marr LU)))
                 (loop for det = (,<t> 0) then (* (expt -1.0 s) det (aref arr (+ i (* i cols))))
                       for i from 0 below rows
                       finally (return det))))))))))

;; [x] mcopy
;; [x] mzero meye mrand
;; [x] miref
;; [x] mcref
;; [x] msetf
;; [x] m= m~= m/= m< m> m<= m>=
;; [x] m+ m- m/
;; [x] m*
;; [ ] mapply mapplyf
;; [x] mdet
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
;; [ ] mlookat mfrustum mortho mperspective
;; [ ] m1norm
;; [ ] minorm
;; [ ] m2norm
;; [ ] mqr
;; [ ] meigen
;; [ ] mcol mrow mblock
;; [ ] mdiag
;; [ ] mswap-row
;; [ ] mswap-col

(do-mat-combinations define-copy)
(do-mat-combinations define-aref)
(do-mat-combinations define-cref)
(do-mat-combinations define-smatop (+ - * / min max) (<t> real))
(do-mat-combinations define-1matop (- / identity))
(do-mat-combinations define-2matreduce (and) (= ~= /= < <= >= >) boolean)
(do-mat-combinations define-smatreduce (and) (= ~= /= < <= >= >) (<t> real) boolean)
(do-mat-combinations define-2matreduce (or) (/=) boolean)
(do-mat-combinations define-smatreduce (or) (/=) (<t> real) boolean)
(do-mat-combinations define-0matop (zero eye rand))
(do-mat-combinations define-m*m)
(do-mat-combinations define-mdet)
