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
               (return-type ,(lisp-type type)))
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

;; FIXME: mat4 x vec3
(define-template m*v <s> <t> (x m n)
  (when (member <s> '(n)) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>))
        (vtype (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(lisp-type vtype) x n)
               (return-type ,(lisp-type vtype)))
      (let ((ma ,(place-form type 'arr 'm))
            (na ,(place-form vtype 'arr 'n))
            (xa ,(place-form vtype 'arr 'x))
            (mc ,(attribute type :cols 'm))
            (mi 0))
        (do-times (xy 0 ,(attribute type :rows 'm) 1 x)
          (let ((c (,<t> 0))
                (mi mi))
            (do-times (xx 0 ,(attribute type :cols 'm) 1)
              (setf c (,<t> (+ c (* (aref ma mi) (aref na xx))))))
            (setf (aref xa xy) c)
            (incf mi 1))
          (incf mi mc))))))

(define-template mdet <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>))
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

(define-template mtranspose <s> <t> (x m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) x m)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x))
            (xc ,(attribute type :cols 'c)))
        (do-times (y 0 ,(attribute type :cols) 1 x)
          (do-times (x 0 ,(attribute type :rows) 1)
            (setf (aref xa (+ y (* x xc))) (aref ma (+ x (* y xc))))))))))

(define-template mtrace <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>)
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (r (,<t> 0)))
        (do-times (i 0 (expt 2 (min ,(attribute type :cols) ,(attribute type :rows))) (1+ ,(attribute type :cols)) r)
          (setf r (+ r (aref ma i))))))))

(define-template minv-affine <s> <t> (x m)
  (unless (eql 4 <s>) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m x)
               (return-type ,(lisp-type type))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form type 'arr 'x)))
        (macrolet ((e (y x) `(aref ma (+ ,x (* ,y 4))))
                   (f (y x) `(aref xa (+ ,x (* ,y 4)))))
          ;; Transpose the 3x3 rotation matrix
          ,@(loop for x from 0 below 3
                  append (loop for y from 0 below 3
                               collect `(setf (f ,y ,x) (e ,x ,y))))
          ;; Transpose the translation vector
          (let ((x (- (e 0 3)))
                (y (- (e 1 3)))
                (z (- (e 2 3))))
            (setf (f 0 3) (,<t> (+ (* x (e 0 0)) (* y (e 0 1)) (* z (e 0 2)))))
            (setf (f 1 3) (,<t> (+ (* x (e 1 0)) (* y (e 1 1)) (* z (e 1 2)))))
            (setf (f 2 3) (,<t> (+ (* x (e 2 0)) (* y (e 2 1)) (* z (e 2 2))))))
          ;; Fill the last row
          (setf (f 3 0) (,<t> 0))
          (setf (f 3 1) (,<t> 0))
          (setf (f 3 2) (,<t> 0))
          (setf (f 3 3) (,<t> 1))
          x)))))

(define-template mtransfer <sx> <s> <t> (x m xy xx w h my mx)
  (let ((type (type-instance 'mat-type <s> <t>))
        (rett (type-instance 'mat-type <sx> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(lisp-type rett) x)
               (type mat-dim xy xx w h my mx)
               (return-type ,(lisp-type rett))
               inline)
      (let* ((ma ,(place-form type 'arr 'm))
             (xa ,(place-form rett 'arr 'x))
             (mc ,(attribute type :cols 'm))
             (xc ,(attribute rett :cols 'x))
             (xi (+ xx (* xc xy)))
             (mi (+ mx (* mc my))))
        (declare (type mat-dim xi mi mc xc))
        (dotimes (_ h x)
          (dotimes (i w)
            (setf (aref xa (+ xi i)) (aref ma (+ mi i))))
          (setf xi (+ xi xc))
          (setf mi (+ mi mc)))))))

(defmacro define-generation-template (name args &body body)
  `(define-template ,name <s> <t> ,args
     (let ((type (type-instance 'mat-type <s> <t>)))
       (flet ((f (&rest args)
                (loop for arg in args
                      for i from 0 for x = (mod i 4) for y = (floor i 4)
                      when (and (< x <s>) (< y <s>))
                      collect `(setf (aref xa ,i) (,<t> ,arg)))))
         `((declare (type ,(lisp-type type) x)
                    (return-type ,(lisp-type type)))
           (let ((xa ,(place-form type 'arr 'x)))
             ,@(progn ,@body))
           x)))))

(define-generation-template mtranslation (x v)
  (when (member <s> '(2 n)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type (case <s> (4 3) (3 2)) <t>)))
    `((declare (type ,(lisp-type vtype) v))
      ,@(case <s>
          (3 (f 1 0 (place-form vtype :x 'v)
                0 1 (place-form vtype :y 'v)
                0 0 1))
          (4 (f 1 0 0 (place-form vtype :x 'v)
                0 1 0 (place-form vtype :y 'v)
                0 0 1 (place-form vtype :z 'v)
                0 0 0 1))))))

(define-generation-template mscaling (x v)
  (when (member <s> '(n)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type (case <s> (4 3) ((3 2) 2)) <t>)))
    `((declare (type ,(lisp-type vtype) v))
      ,@(case <s>
          (2 (f (place-form vtype :x 'v) 0
                0 (place-form vtype :y 'v)))
          (3 (f (place-form vtype :x 'v) 0 0
                0 (place-form vtype :y 'v) 0
                0 0 1))
          (4 (f (place-form vtype :x 'v) 0 0 0
                0 (place-form vtype :y 'v) 0 0
                0 0 (place-form vtype :z 'v) 0
                0 0 0 1))
          (T (template-unfulfillable))))))

(define-generation-template mrotation (x v angle)
  (when (member <s> '(n)) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type vtype) v)
               (type ,<t> angle))
      (let ((c (,<t> (cos angle)))
            (s (,<t> (sin angle))))
        ,@(case <s>
            (2 (f 'c '(- s)
                  's 'c))
            (T `((cond ((v= +vx3+ v)
                        ,@(f 1 0 0 0
                             0 'c '(- s) 0
                             0 's 'c 0
                             0 0 0 1))
                       ((v= +vy3+ v)
                        ,@(f 'c 0 's 0
                             0 1 0 0
                             '(- s) 0 'c 0
                             0 0 0 1))
                       ((v= +vz3+ v)
                        ,@(f 'c '(- s) 0 0
                             's 'c 0 0
                             0 0 1 0
                             0 0 0 1))
                       (T
                        ;; https://joombig.com/sqlc/3D-Rotation-Algorithm-about-arbitrary-axis-with-CC-code-tutorials-advance
                        (let* ((x ,(place-form vtype :x 'v))
                               (y ,(place-form vtype :y 'v))
                               (z ,(place-form vtype :z 'v))
                               (1-c (- 1 c))
                               (u2 (expt x 2))
                               (v2 (expt y 2))
                               (w2 (expt z 2))
                               (l (+ u2 v2 w2))
                               (sqrtl (sqrt l)))
                          ,@(f '(/ (+ u2 (* (+ v2 w2) c)) l)        '(/ (- (* x y 1-c) (* z sqrtl s)) l) '(/ (+ (* x z 1-c) (* y sqrtl s)) l) 0
                               '(/ (+ (* x y 1-c) (* z sqrtl s)) l) '(/ (+ v2 (* (+ u2 w2) c)) l)        '(/ (- (* y z 1-c) (* x sqrtl s)) l) 0
                               '(/ (- (* x z 1-c) (* y sqrtl s)) l) '(/ (+ (* y z 1-c) (* x sqrtl s)) l) '(/ (+ w2 (* (+ u2 v2) c)) l)        0
                               0                                   0                                   0                                   1)))))))))))

(define-generation-template mlookat (x eye target up)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  (let ((vtype (type-instance 'vec-type 3 <t>)))
    `((declare (type ,(lisp-type vtype) eye target up))
      (let* ((z (nvunit (v- eye target)))
             (x (nvunit (vc up z)))
             (y (vc z x)))
        ,@(f (place-form vtype :x 'x) (place-form vtype :y 'x) (place-form vtype :z 'x) '(- (v. x eye))
             (place-form vtype :x 'y) (place-form vtype :y 'y) (place-form vtype :z 'y) '(- (v. y eye))
             (place-form vtype :x 'z) (place-form vtype :y 'z) (place-form vtype :z 'z) '(- (v. z eye))
             0 0 1)))))

(define-generation-template mfrustum (x l r b u n f)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  `((declare (type ,<t> l r b u n f))
    ,@(f '(/ (* 2 n) (- r l)) 0                    '(/ (+ r l) (- r l))     0
         0                    '(/ (* 2 n) (- u b)) '(/ (+ u b) (- u b))     0
         0                    0                    '(- (/ (+ f n) (- f n))) '(/ (* -2 f n) (- f n))
         0                    0                    -1                       0)))

(define-generation-template mortho (x l r b u n f)
  (unless (eql <s> 4) (template-unfulfillable))
  (when (member <t> '(i32 u32)) (template-unfulfillable))
  `((declare (type ,<t> l r b u n f))
    ,@(f '(/ 2 (- r l)) 0              0              '(- (/ (+ r l) (- r l)))
         0              '(/ 2 (- u b)) 0              '(- (/ (+ u b) (- u b)))
         0              0             '(/ -2 (- f n)) '(- (/ (+ f n) (- f n)))
         0              0              0              1)))

(define-template m1norm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>)
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (mc ,(attribute type :cols))
            (max (,<t> 0)))
        (do-times (x 0 ,(attribute type :cols) 1 max)
          (let ((col (,<t> 0)))
            (do-times (y 0 ,(attribute type :rows) 1)
              (setf col (+ col (abs (aref ma (+ x (* y mc)))))))
            (when (< max col)
              (setf max col))))))))

(define-template minorm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>)
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (mc ,(attribute type :cols))
            (max (,<t> 0)))
        (do-times (y 0 ,(attribute type :rows) 1 max)
          (let ((col (,<t> 0)))
            (do-times (x 0 ,(attribute type :cols) 1)
              (setf col (+ col (abs (aref ma (+ x (* y mc)))))))
            (when (< max col)
              (setf max col))))))))

(define-template m2norm <s> <t> (m)
  (let ((type (type-instance 'mat-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (return-type ,<t>)
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (sum (,<t> 0)))
        (do-times (i 0 ,(attribute type :len) 1 sum)
          (setf sum (+ sum (expt (aref ma i) 2))))))))

(define-template mdiag <s> <t> (x m)
  (when (eql <s> 'n) (template-unfulfillable))
  (let ((type (type-instance 'mat-type <s> <t>))
        (vtype (type-instance 'vec-type <s> <t>)))
    `((declare (type ,(lisp-type type) m)
               (type ,(lisp-type vtype) x)
               (return-type ,(lisp-type vtype))
               inline)
      (let ((ma ,(place-form type 'arr 'm))
            (xa ,(place-form vtype 'arr 'x)))
        ,@(loop for i from 0 below <s>
                collect `(setf (aref xa ,i) (aref ma ,(+ i (* i <s>)))))
        x))))

;; [x] mcopy
;; [x] mzero meye mrand
;; [x] miref
;; [x] mcref
;; [x] msetf
;; [x] m= m~= m/= m< m> m<= m>=
;; [x] m+ m- m/
;; [x] m*
;; [x] mdet
;; [x] mtranspose
;; [x] mtrace
;; [ ] minv
;; [x] minv-affine
;; [ ] mminor
;; [ ] mcofactor
;; [ ] mcof
;; [ ] madj
;; [ ] mpivot
;; [ ] mlu
;; [ ] mqr
;; [x] mtranslation
;; [x] mscaling
;; [x] mrotation
;; [x] mlookat
;; [x] mfrustum
;; [x] mortho
;; [/] mperspective
;; [x] m1norm
;; [x] minorm
;; [x] m2norm
;; [/] meigen
;; [/] mblock mcol mrow mswap-row mswap-col
;; [x] mdiag

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
(do-mat-combinations define-m*v)
(do-mat-combinations define-mdet)
(do-mat-combinations define-minv-affine)
(do-mat-combinations define-mtranspose)
(do-mat-combinations define-mtrace)
(do-mat-combinations define-mtransfer (2 3 4 n))
(do-mat-combinations define-mtranslation)
(do-mat-combinations define-mscaling)
(do-mat-combinations define-mrotation)
(do-mat-combinations define-mlookat)
(do-mat-combinations define-mfrustum)
(do-mat-combinations define-mortho)
(do-mat-combinations define-m1norm)
(do-mat-combinations define-minorm)
(do-mat-combinations define-m2norm)
(do-mat-combinations define-mdiag)
