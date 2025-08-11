(asdf:defsystem 3d-matrices-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-matrices system."
  :homepage "https://shinmera.com/docs/3d-matrices/"
  :bug-tracker "https://shinmera.com/project/3d-matrices/issues"
  :source-control (:git "https://shinmera.com/project/3d-matrices.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-matrices :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3d-matrices-test)))
