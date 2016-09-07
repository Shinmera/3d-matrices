#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem 3d-matrices
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing 2x2, 3x3, 4x4, and NxN matrix functionality."
  :homepage "https://github.com/Shinmera/3d-matrices"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "struct")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors))
