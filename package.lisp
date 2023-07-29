#|
 This file is a part of 3d-matrices
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:3d-matrices
  (:nicknames #:org.shirakumo.fraf.matrices)
  (:use #:cl #:org.shirakumo.type-templates #:org.shirakumo.fraf.vectors)
  (:import-from #:org.shirakumo.fraf.vectors
                #:f32 #:f64 #:u32 #:i32 #:type-prefix #:vec-type #:arr #:~=)
  (:local-nicknames
   (#:v #:org.shirakumo.fraf.vectors)))
