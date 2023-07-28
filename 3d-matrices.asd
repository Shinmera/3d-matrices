#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem 3d-matrices
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing 2x2, 3x3, 4x4, and NxN matrix functionality."
  :homepage "https://Shinmera.github.io/3d-matrices/"
  :bug-tracker "https://github.com/Shinmera/3d-matrices/issues"
  :source-control (:git "https://github.com/Shinmera/3d-matrices.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "types")
               (:file "raw-ops")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-matrices-test))))
