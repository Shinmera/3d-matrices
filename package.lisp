#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:3d-matrices
  (:nicknames #:org.shirakumo.flare.matrix)
  (:use #:cl #:3d-vectors)
  ;; ops.lisp
  (:export
   #:with-fast-matref
   #:with-fast-matrefs
   #:with-fast-matcase
   #:meye
   #:mrand
   #:muniform
   #:mcol
   #:mrow
   #:m=
   #:m~=
   #:m/=
   #:m<
   #:m>
   #:m<=
   #:m>=
   #:m+
   #:nm+
   #:m-
   #:nm-
   #:m*
   #:nm*
   #:n*m
   #:m/
   #:nm/
   #:mapply
   #:mapplyf
   #:mdet
   #:minv
   #:mtranspose
   #:mtrace
   #:mcofactor
   #:mcof
   #:madj
   #:mpivot
   #:mlu
   #:mtranslation
   #:mscaling
   #:mrotation
   #:nmtranslate
   #:nmscale
   #:nmrotate
   #:m1norm
   #:minorm
   #:m2norm
   #:mqr
   #:meigen
   #:nmswap-row
   #:nmswap-col
   #:mdiag
   #:mblock
   #:mtop
   #:mbottom
   #:mleft
   #:mright)
  ;; struct.lisp
  (:export
   #:mat2
   #:mat2-p
   #:marr2
   #:miref2
   #:mcref2
   #:mcopy2
   #:mat3
   #:mat3-p
   #:marr3
   #:miref3
   #:mcref3
   #:mcopy3
   #:mat4
   #:mat4-p
   #:marr4
   #:miref4
   #:mcref4
   #:mcopy4
   #:matn
   #:matn-p
   #:marrn
   #:mirefn
   #:mcrefn
   #:mcopyn
   #:mat
   #:mat-p
   #:marr
   #:miref
   #:mcref
   #:mcols
   #:mrows
   #:mat
   #:matf
   #:mcopy
   #:write-matrix))
