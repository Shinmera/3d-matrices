(in-package #:org.shirakumo.flare.matrix)

(defmacro %mka (&environment env size &key (element 0) contents (coercion '#'ensure-float))
  (let ((arr (gensym "ARRAY")))
    `(let ((,arr (make-array ,size :element-type ',*float-type*
                                   :initial-element ,(ensure-float-param element env))))
       ,(if contents
            `(map-into ,arr ,coercion ,contents)
            arr))))

(defmacro %proper-array (size elements)
  `(etypecase ,elements
     (null (%mka ,size))
     (real (%mka ,size :element ,elements))
     (sequence (%mka ,size :contents ,elements))))

(defun %proper-array-form (size elements)
  (let ((el (gensym "ELEMENTS")))
    (cond ((null elements)
           `(%mka ,size))
          ((numberp elements)
           `(%mka ,size :element ,elements))
          ((or (typep elements 'vector)
               (and (typep elements 'list)
                    (eql (first elements) 'quote)
                    (listp (second elements))))
           `(%mka ,size :contents (load-time-value
                                   (map 'list #'ensure-float ,elements))
                        :coercion #'identity))
          ;; There's other cases but we can't really catch them because typep is not
          ;; actually useful. Ah well, this will have to be good enough for most.
          (T
           `(let ((,el ,elements))
              (%proper-array ,size ,el))))))

(defmacro define-describe-matrix (type)
  `(defmethod describe-object ((a ,type) stream)
     (format stream "~ax~a matrix of type ~a~%~%"
             (mrows a) (mcols a) ',type)
     (write-matrix a stream)))

(defmacro define-mat-accessor (mtype miref mcref marr size)
  `(progn
     (declaim (inline ,miref))
     (declaim (ftype (function (,mtype (integer 0 ,(1- (* size size)))) float-type) ,miref))
     (define-ofun ,miref (mat i)
       #+lispworks
       (unless (< -1 i (length (,marr mat)))
         (error "~d is out of bounds for ~a" i mat))
       (aref (,marr mat) i))

     (defsetf* ,miref (mat i &environment env) (value)
       `(progn
          #+lispworks
          (unless (< -1 ,i (length (,',marr ,mat)))
            (error "~d is out of bounds for ~a" ,i ,mat))
          (setf (aref (,',marr ,mat) ,i) ,(ensure-float-param value env))))

     (declaim (inline ,mcref))
     (declaim (ftype (function (,mtype (integer 0 ,(1- size)) (integer 0 ,(1- size))) float-type) ,mcref))
     (define-ofun ,mcref (mat y x)
       (let ((i (+ (* y ,size) x)))
         #+lispworks
         (unless (< -1 i (length (,marr mat)))
           (error "~d,~d is out of bounds for ~a" y x mat))
         (aref (,marr mat) i)))
     
     (defsetf* ,mcref (mat y x &environment env) (value)
       `(let ((#1=#.(gensym "I") (+ (* ,y ,,size) ,x)))
          #+lispworks
          (unless (< -1 i (length (,',marr 'mat)))
            (error "~d,~d is out of bounds for ~a" ,y ,x ,mat))
          (setf (aref (,',marr ,mat) #1#) ,(ensure-float-param value env))))))

(declaim (inline %mat2))
(defstruct (mat2 (:conc-name NIL)
                 (:constructor %mat2 (marr2))
                 (:copier NIL)
                 (:predicate mat2-p))
  (marr2 NIL :type (simple-array float-type (4))))

(define-mat-accessor mat2 miref2 mcref2 marr2 2)

(define-ofun mat2 (&optional elements)
  (%mat2 (%proper-array 4 elements)))

(define-compiler-macro mat2 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat2 ,(%proper-array-form 4 elements)))
        (T whole)))

(declaim (inline mcopy2))
(declaim (ftype (function (mat2) mat2) mcopy2))
(defun mcopy2 (m2)
  (%mat2 (copy-seq (marr2 m2))))

(defmethod print-object ((m mat2) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat2) &optional env)
  (declare (ignore env))
  `(mat2 ,(marr2 m)))

(define-describe-matrix mat2)

(declaim (inline %mat3))
(defstruct (mat3 (:conc-name NIL)
                 (:constructor %mat3 (marr3))
                 (:copier NIL)
                 (:predicate mat3-p))
  (marr3 NIL :type (simple-array float-type (9))))

(define-mat-accessor mat3 miref3 mcref3 marr3 3)

(define-ofun mat3 (&optional elements)
  (%mat3 (%proper-array 9 elements)))

(define-compiler-macro mat3 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat3 ,(%proper-array-form 9 elements)))
        (T whole)))

(declaim (inline mcopy3))
(declaim (ftype (function (mat3) mat3) mcopy3))
(defun mcopy3 (m3)
  (%mat3 (copy-seq (marr3 m3))))

(defmethod print-object ((m mat3) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat3) &optional env)
  (declare (ignore env))
  `(mat3 ,(marr3 m)))

(define-describe-matrix mat3)

(declaim (inline %mat4))
(defstruct (mat4 (:conc-name NIL)
                 (:constructor %mat4 (marr4))
                 (:copier NIL)
                 (:predicate mat4-p))
  (marr4 NIL :type (simple-array float-type (16))))

(define-mat-accessor mat4 miref4 mcref4 marr4 4)

(define-ofun mat4 (&optional elements)
  (%mat4 (%proper-array 16 elements)))

(define-compiler-macro mat4 (&whole whole &environment env &optional elements)
  (cond ((constantp elements env)
         `(%mat4 ,(%proper-array-form 16 elements)))
        (T whole)))

(declaim (inline mcopy4))
(declaim (ftype (function (mat4) mat4) mcopy4))
(defun mcopy4 (m4)
  (%mat4 (copy-seq (marr4 m4))))

(defmethod print-object ((m mat4) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m mat4) &optional env)
  (declare (ignore env))
  `(mat4 ,(marr4 m)))

(define-describe-matrix mat4)

(defstruct (matn (:conc-name NIL)
                 (:constructor %matn (%rows %cols marrn))
                 (:copier NIL)
                 (:predicate matn-p))
  (%rows NIL :type mat-dim)
  (%cols NIL :type mat-dim)
  (marrn NIL :type (simple-array float-type)))

(declaim (inline mirefn))
(declaim (ftype (function (matn (integer 0 #.(expt *matrix-limit* 2))) float-type) mirefn))
(define-ofun mirefn (mat i)
  (aref (marrn mat) i))

(defsetf* mirefn (mat i &environment env) (value)
  `(setf (aref (marrn ,mat) ,i) ,(ensure-float-param value env)))

(declaim (inline mcrefn))
(declaim (ftype (function (matn mat-dim mat-dim) float-type) mcrefn))
(define-ofun mcrefn (mat y x)
  (aref (marrn mat) (+ (* y (%cols mat)) x)))

(defsetf* mcrefn (mat y x &environment env) (value)
  `(setf (aref (marrn ,mat) (+ (* ,y (%cols ,mat)) ,x)) ,(ensure-float-param value env)))

(define-ofun matn (r c &optional elements)
  (check-type r mat-dim)
  (check-type c mat-dim)
  (let ((arr (%proper-array (* r c) elements)))
    (if (= r c)
        (case r
          (2 (%mat2 arr))
          (3 (%mat3 arr))
          (4 (%mat4 arr))
          (T (%matn r r arr)))
        (%matn r c arr))))

(define-compiler-macro matn (&whole whole &environment env r c &optional elements)
  (cond ((constantp elements env)
         (let ((arr (gensym "ARR")) (rows (gensym "ROWS")) (cols (gensym "COLS")))
           `(let ((,arr ,(%proper-array-form `(* ,c ,r) elements))
                  (,rows ,r) (,cols ,c))
              (if (= ,rows ,cols)
                  (case ,rows
                    (2 (%mat2 ,arr))
                    (3 (%mat3 ,arr))
                    (4 (%mat4 ,arr))
                    (T (%matn ,rows ,rows ,arr)))
                  (%matn ,rows ,cols ,arr)))))
        (T whole)))

(declaim (inline mcopyn))
(declaim (ftype (function (matn) matn) mcopyn))
(defun mcopyn (mn)
  (%matn (%rows mn) (%cols mn) (copy-seq (marrn mn))))

(defmethod print-object ((m matn) stream)
  (write (make-load-form m) :stream stream))

(defmethod make-load-form ((m matn) &optional env)
  (declare (ignore env))
  `(matn ,(%rows m) ,(%cols m) ,(marrn m)))

(define-describe-matrix matn)

;; Compat
(deftype mat ()
  `(or mat2 mat3 mat4 matn))

(declaim (inline mat-p))
(declaim (ftype (function (mat) boolean) mat-p))
(defun mat-p (mat)
  (typep mat 'mat))

(declaim (inline marr))
(declaim (ftype (function (mat) (simple-array float-type)) marr))
(defun marr (mat)
  (etypecase mat
    (mat2 (marr2 mat))
    (mat3 (marr3 mat))
    (mat4 (marr4 mat))
    (matn (marrn mat))))

(declaim (inline miref))
(declaim (ftype (function (mat (integer 0 #.(expt *matrix-limit* 2))) float-type) miref))
(defun miref (mat i)
  (etypecase mat
    (mat2 (miref2 mat i))
    (mat3 (miref3 mat i))
    (mat4 (miref4 mat i))
    (matn (mirefn mat i))))

(declaim (inline mcref))
(declaim (ftype (function (mat mat-dim mat-dim) float-type) mcref))
(defun mcref (mat y x)
  (etypecase mat
    (mat2 (mcref2 mat y x))
    (mat3 (mcref3 mat y x))
    (mat4 (mcref4 mat y x))
    (matn (mcrefn mat y x))))

(defsetf* miref (mat i) (value)
  `(etypecase ,mat
     (mat2 (setf (miref2 ,mat ,i) ,value))
     (mat3 (setf (miref3 ,mat ,i) ,value))
     (mat4 (setf (miref4 ,mat ,i) ,value))
     (matn (setf (mirefn ,mat ,i) ,value))))

(defsetf* mcref (mat y x) (value)
  `(etypecase ,mat
     (mat2 (setf (mcref2 ,mat ,y ,x) ,value))
     (mat3 (setf (mcref3 ,mat ,y ,x) ,value))
     (mat4 (setf (mcref4 ,mat ,y ,x) ,value))
     (matn (setf (mcrefn ,mat ,y ,x) ,value))))

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
            (psetf
             ,@(loop for i from 0 below 4 for v in vals
                     append `((aref (marr2 ,m) ,i) ,(ensure-float-param v env))))
            ,m))
      (9 `(let ((,m (mat3)))
            (psetf
             ,@(loop for i from 0 below 9 for v in vals
                     append `((aref (marr3 ,m) ,i) ,(ensure-float-param v env))))
            ,m))
      (16 `(let ((,m (mat4)))
             (psetf
              ,@(loop for i from 0 below 16 for v in vals
                      append `((aref (marr4 ,m) ,i) ,(ensure-float-param v env))))
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
            (psetf
             ,@(loop for i from 0 below 4 for v in vals
                     append `((aref (marr2 ,m) ,i) ,(ensure-float-param v env))))
            ,m))
      (9 `(let ((,m ,mat))
            (psetf
             ,@(loop for i from 0 below 9 for v in vals
                     append `((aref (marr3 ,m) ,i) ,(ensure-float-param v env))))
            ,m))
      (16 `(let ((,m ,mat))
             (psetf
              ,@(loop for i from 0 below 16 for v in vals
                      append `((aref (marr4 ,m) ,i) ,(ensure-float-param v env))))
             ,m))
      (T whole))))

(declaim (inline mcopy))
(declaim (ftype (function (mat) mat) mcopy))
(defun mcopy (m)
  (etypecase m
    (mat2 (mcopy2 m))
    (mat3 (mcopy3 m))
    (mat4 (mcopy4 m))
    (matn (mcopyn m))))

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
