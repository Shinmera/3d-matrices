(asdf:defsystem 3d-matrices
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing 2x2, 3x3, 4x4, and NxN matrix functionality."
  :homepage "https://Shinmera.github.io/3d-matrices/"
  :bug-tracker "https://github.com/Shinmera/3d-matrices/issues"
  :source-control (:git "https://github.com/Shinmera/3d-matrices.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "struct")
               (:file "iterate")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-matrices-test))))
