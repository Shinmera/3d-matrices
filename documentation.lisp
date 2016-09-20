#|
 This file is a part of 3d-matrices
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.matrix)

;; iterate.lisp
(docs:define-docs
  (function map-mat-diag
    "Maps the diagonal of the matrix to the function.

The function receives two arguments: the index in the diagonal,
and the element at that position of the matrix.")

  (function map-mat-index
    "Maps the matrix by index to the function.

The function receives two arguments: the row-major index in the
matrix, and the element at that position of the matrix.")

  (function do-mat-diag
    "Iterates over the diagonal of the matrix.

Binds the diagonal index to the variable of I
and the element at that place to the variable of EL.")

  (function do-mat-index
    "Iterates over the indexes of the matrix.

Binds the row-major index to the variable of I
and the element at that place to the variable of EL."))

;; ops.lisp
(docs:define-docs
  (function with-fast-matref
    "Allows efficient referencing to matrix elements.

ACCESSOR designates the name of the local macro that will allow
you to both read and set the matrix element at the given position
It will take either one or two arguments. If two, they are row and
column of the cell to dereference, and if one, it is the row-major
index of the element to dereference.

You must also designate the proper number of columns stored in the
matrix. This is done because often times when you will want to use
this macro, you'll already know the number of columns anyway.
Retrieving it again would be wasteful.

You should use this whenever you need to reference elements in a loop
or need to do more than one reference overall.")

  (function with-fast-matrefs
    "Allows efficient referencing of multiple matrices.

Each binding must be of the form that WITH-FAST-MATREF expects.

See WITH-FAST-MATREF")

  (function with-fast-matcase
    "Does an etypecase on MAT and an appropriate WITH-FAST-MATREF on each case.

The body should be an expression of (MAT-TYPE form*). For each
matrix type /except/ for MATN, the forms will be wrapped in an
appropriate WITH-FAST-MATREF. This is not done for MATN, as often
times a different approach to manipulating the matrix than by
direct reference is preferable for that case.

See WITH-FAST-MATREF")

  (function meye
    "Constructs a square identity matrix of the requested size.")

  (function mrand
    "Constructs a matrix of the requested size where each element is randomized.

MIN and MAX return the inclusive bounds of the numbers in the matrix.")

  (function muniform
    "Constructs a matrix of the requested size where each element is initialised to the requested element.")

  (function mcol
    "Accesses the requested column as a vector of the appropriate size.

This only works for MAT2, MAT3, MAT4.")

  (function mrow
    "Accesses the requested row as a vector of the appropriate size.

This only works for MAT2, MAT3, MAT4.")

  (function %2mat-op
    "Macro for the handling of an operation for two matrices.

A B       --- Variables for the two matrices.
C         --- The function that combines an element from each matrix.
M2 M3 M4  --- The aggregator for the respective matrix type that combines
              the result of the combinator for each element of the matrix
              into one.
MNMN      --- The form to use to calculate a MATN MATN op.
MNR       --- The form to use to calculate a MATN REAL op.")

  (function define-matcomp
    "Defines a matrix comparison function of NAME, using OP as the element-wise comparator.

COMB defines how the elements are combined together, usually by AND.")

  (function m=
    "Compares matrices against each other or a real, returning T if they are element-wise equal by =.")

  (function m~=
    "Compares matrices against each other or a real, returning T if they are element-wise equal by ~=.

See ~=")

  (function m/=
    "Compares matrices against each other or a real, returning T if they are element-wise equal by /=.")

  (function m<
    "Compares matrices against each other or a real, returning T if they are element-wise ordered by <.")

  (function m>
    "Compares matrices against each other or a real, returning T if they are element-wise ordered by >.")

  (function m<=
    "Compares matrices against each other or a real, returning T if they are element-wise ordered by <=.")

  (function m>=
    "Compares matrices against each other or a real, returning T if they are element-wise ordered by >=.")

  (function define-matop
    "Defines a non-mutating matrix operation of NAME that delegates to NNAME for secondary calculations, and uses OP to combine elements.

If BODY is given, it is used for the 2MAT-case in place of a standard %2MAT-OP based approach.
Body will be called with two arguments, the A and B matrix of the combination.

A MATOP like this always returns a fresh matrix.")

  (function define-nmatop
    "Defines a mutating matrix operation of NAME that uses OP to combine elements.

If BODY is given, it is used for the 2MAT-case in place of standard %2MAT-OP based approach.
Body will be called with two arguments, the A and B matrix of the combination.

A MATOP like this always returns the modified, first matrix.")

  (function m+
    "Computes the element-wise addition of the matrices or reals. Returns a fresh matrix.")

  (function nm+
    "Computes the element-wise addition of the matrices or reals. Returns the first matrix, modified.")

  (function m-
    "Computes the element-wise subtraction of the matrices or reals. Returns a fresh matrix.")

  (function nm-
    "Computes the element-wise subtraction of the matrices or reals. Returns the first matrix, modified.")

  (function %2mat*-expansion
    "Inner expansion to compute a proper matrix multiplication, which is non element-wise.")

  (function %2nmat*-expansion
    "Inner expansion to compute a proper, modifying matrix multiplication, which is non element-wise.")

  (function %2n*mat-expansion
    "Inner expansion to compute a proper, modifying matrix multiplication on the right side.")

  (function m*
    "Computes a matrix multiplication.

If the other operand is a real, the matrix is multiplied with the real element-wise.
If the other operand is a matrix, they are multiplied as per matrix multiplication.
Note that the returned matrix may have different size than the input matrices as a result
of this. The two matrices must agree on the size as per matrix multiplication.")

  (function nm*
    "Computes a modifying matrix multiplication.

If the other operand is a real, the matrix is multiplied with the real element-wise.
If the other operand is a matrix, they are multiplied as per matrix multiplication.
Note that this only works for square matrix against square matrix, as otherwise a size
change would occur, which is not possible to do in a modifying variant. The two matrices
must agree on the size as per matrix multiplication.
If the other operand is a vector, the vector is modified.

See N*M")

  (function n*m
    "Computes a modifying matrix multiplication, but modifying the right-hand side.

See NM*")

  (function %2mat/-expansion
    "Inner expansion to compute an element-wise matrix division.")

  (function %2nmat/-expansion
    "Inner expansion to compute an element-wise, modifying matrix division.")

  (function m/
    "Computes an element-wise division of the matrix from a real. Returns a fresh matrix.")

  (function nm/
    "Computes an element-wise division of the matrix from a real. Returns the modified, first matrix.")

  (function mapply
    "Applies the function to each element of the matrix and maps the result of it to a new matrix.")

  (function mapplyf
    "Applies the function to each element of the matrix and maps the result of it back into the matrix.")

  (function mdet
    "Computes the determinant of the matrix.

For MAT2 MAT3 MAT4, inlined variants exist. For MATN, an algorithm based on LU factorisation is used.")

  (function minv
    "Computes the inverses of the matrix.

This is only possible if the determinant is non-zero.

For MAT2 MAT3 MAT4, inlined variants exist. For MATN, an algorithm based on the adjugate is used.")

  (function mtranspose
    "Computes the transpose of the matrix.

For MAT2 MAT3 MAT4, inlined variants exist. For MATN, a generic swap is used.")

  (function mtrace
    "Computes the trace of the matrix.

For MAT2 MAT3 MAT4, inlined variants exist. For MATN, a generic sum is used.")

  (function mcofactor
    "Computes the cofactor at the specified index of the matrix.")

  (function mcof
    "Computes the cofactor matrix.

See MCOFACTOR")

  (function madj
    "Computes the adjugate of the matrix.

For MAT2 MAT3 MAT4, inlined variants exist. For MATN, an algorithm based on the cofactors is used.")

  (function mpivot
    "Attempts to do a partial pivotisation.

Returns the pivotised matrix, the permutation matrix, and the number of permutations that were done.")

  (function mlu
    "Computes an LU factorisation of the matrix.

An approach based on Crout is used with on-the-fly pivotisation if requested.

Returns the combined LU matrix, the permutation matrix, and the number of permutations that were done.")

  (function mtranslation
    "Returns a 3D translation matrix for the given vector as a MAT4.")

  (function mscaling
    "Returns a 3D scaling matrix for the given vector as a MAT4.")

  (function mrotation
    "Returns a 3D rotation matrix for the given vector as a MAT4.")

  (function nmtranslate
    "Translates the given matrix by the vector. Returns the modified matrix.")

  (function nmscale
    "Scales the given matrix by the vector. Returns the modified matrix.")

  (function nmrotate
    "Rotates the given matrix by the vector. Returns the modified matrix.")

  (function m1norm
    "Computes the 1 norm of the matrix, namely the maximum of the sums of the columns.")

  (function minorm
    "Computes the infinity norm of the matrix, namely the maximum of the sums of the rows.")

  (function m2norm
    "Computes the 2 norm of the matrix, namely the square root of the sum of all squared elements.")

  (function mqr
    "Computes the QR factorisation of the matrix.

An approach based on givens rotations is used.

Returns the Q and R matrices, which are fresh.")

  (function meigen
    "Computes an approximation of the eigenvalues of the matrix.

An approach based on QR factorisation is used. The number of iterations dictates
how many times the factorisation is repeated to make the result more accurate.
Usually something around 50 iterations should give somewhat accurate results, but
due to floating point limitations that may be off more significantly.

Returns the eigenvalues as a list.")

  (function nmswap-row
    "Modifies the matrix by swapping the Kth row with the Lth row.")

  (function nmswap-col
    "Modifies the matrix by swapping the Kth column with the Lth column.")

  (function mswap-row
    "Returns a copy of the matrix with the rows swapped.")

  (function mswap-col
    "Returns a copy of the matrix with the columns swapped.")

  (function mdiag
    "Returns the diagonal values of the matrix as a list.")

  (function mblock
    "Returns the designated sub-matrix as a new matrix.
Y1, X1 are the upper left corner, inclusive
Y2, X2 are the lower right corner, exclusive")

  (function mtop
    "Returns the topmost N rows as a new matrix.")

  (function mbottom
    "Returns the lowermost N rows as a new matrix.")

  (function mleft
    "Returns the leftmost N columns as a new matrix.")

  (function mright
    "Returns the rightmost N columns as a new matrix."))

;; struct.lisp
(docs:define-docs
  (function %mka
    "Proper array construction helper.

Emits a form that returns a useable array for a matrix.")

  (function %proper-array
    "Proper array constructor.

ELEMENTS can be
  NULL     --- The initial element is set to 0.
  REAL     --- The initial element is set to this.
  SEQUENCE --- The array is mapped into with this sequence
               and the initial element is set to 0.")

  (function %proper-array-form
    "Returns a form that should construct a proper array for the given compile-time args.")

  (function define-describe-matrix
    "Shorthand macro to define a describe-object method.")

  (type mat2
    "The 2x2 matrix type.")

  (function %mat2
    "Direct constructor for the 2x2 matrix struct.")

  (function marr2
    "Direct accessor to the backing array of the MAT2.")

  (function mat2-p
    "Returns T if the given object is of type MAT2.")

  (function miref2
    "Returns the element at the given index in the MAT2.

Elements are stored in row-major format.")

  (function mcref2
    "Returns the element at the given cell in the MAT2.")

  (function mat2
    "Constructs a MAT2 from the given elements.

ELEMENTS can be
  NULL     --- The matrix is initialised with zeroes.
  REAL     --- The matrix is initialised with this number.
  SEQUENCE --- The sequence is mapped into the matrix
               and the rest are initialised to 0.")

  (function mcopy2
    "Creates a full copy of the MAT2.")

  (type mat3
    "The 3x3 matrix type.")

  (function %mat3
    "Direct constructor for the 3x3 matrix struct.")

  (function marr3
    "Direct accessor to the backing array of the MAT3o.")

  (function mat3-p
    "Returns T if the given object is of type MAT3.")

  (function miref3
    "Returns the element at the given index in the MAT3.

Elements are stored in row-major format.")

  (function mcref3
    "Returns the element at the given cell in the MAT3.")

  (function mat3
    "Constructs a MAT3 from the given elements.

ELEMENTS can be
  NULL     --- The matrix is initialised with zeroes.
  REAL     --- The matrix is initialised with this number.
  SEQUENCE --- The sequence is mapped into the matrix
               and the rest are initialised to 0.")

  (function mcopy3
    "Creates a full copy of the MAT3.")

  (type mat4
    "The 4x4 matrix type.")

  (function %mat4
    "Direct constructor for the 4x4 matrix struct.")

  (function marr4
    "Direct accessor to the backing array of the MAT4.")

  (function mat4-p
    "Returns T if the given object is of type MAT4.")

  (function miref4
    "Returns the element at the given index in the MAT4.

Elements are stored in row-major format.")

  (function mcref4
    "Returns the element at the given cell in the MAT4.")

  (function mat4
    "Constructs a MAT4 from the given elements.

ELEMENTS can be
  NULL     --- The matrix is initialised with zeroes.
  REAL     --- The matrix is initialised with this number.
  SEQUENCE --- The sequence is mapped into the matrix
               and the rest are initialised to 0.")

  (function mcopy4
    "Creates a full copy of the MAT4.")

  (type matn
    "The NxM matrix type.")

  (function %matn
    "Direct constructor for the NxM matrix struct.")

  (function marrn
    "Direct accessor to the backing array of the MATN.")

  (function %cols
    "Direct accessor to the number of columns in the MATN.")

  (function %rows
    "Direct accessor to the number of rows in the MATN.")

  (function matn-p
    "Returns T if the given object is of type MATN.")

  (function mirefn
    "Returns the element at the given index in the MATN.

Elements are stored in row-major format.")

  (function mcrefn
    "Returns the element at the given cell in the MATN.")

  (function matn
    "Constructs a MATN of the requested size from the given elements.

ELEMENTS can be
  NULL     --- The matrix is initialised with zeroes.
  REAL     --- The matrix is initialised with this number.
  SEQUENCE --- The sequence is mapped into the matrix
               and the rest are initialised to 0.

Note that if R and C are both...
  2 - A MAT2 is constructed
  3 - A MAT3 is constructed
  4 - A MAT4 is constructed
instead of a MATN.")

  (function mcopyn
    "Creates a full copy of the MATN.")
  
  (type mat
    "Supertype for all matrix types.

See MAT2
See MAT3
See MAT4
See MATN")

  (function mat-p
    "Returns T if the given object is of type MAT.

See MAT2-P
See MAT3-P
See MAT4-P
See MATN-P")

  (function marr
    "Returns the backing array used by the matrix.

This should be a SIMPLE-VECTOR with the elements of type FLOAT-TYPE.

See MARR2
See MARR3
See MARR4
See MARRN")

  (function miref
    "Returns the element at the given index in the matrix.

Elements are stored in row-major format.

See MIREF2
See MIREF3
See MIREF4
See MIREFN")

  (function mcref
    "Returns the element at the given cell in the matrix.

See MCREF2
See MCREF3
See MCREF4
See MCREFN")

  (function mcols
    "Returns the number of columns the matrix stores.")

  (function mrows
    "Returns the number of rows the matrix stores.")

  (function mat
    "Constructs a fitting matrix for the number of elements given.

This only works for square numbers of elements, as otherwise it is not possible
to guess what dimensions the matrix should have. In the case of a non-square
number, an error is signalled.

See MAT2
See MAT3
See MAT4
See MATN")

  (function matf
    "Maps the VALs into the matrix.

The values will be mapped in row-major order.")

  (function mcopy
    "Creates a full copy of the matrix.

See MCOPY2
See MCOPY3
See MCOPY4
See MCOPYN")

  (function write-matrix
    "Writes the matrix in a certain format, by default a human-readable one.

FORMAT can be one of
  :NICE    - Prints it in a nice representation intended for humans.
  :WOLFRAM - Prints it in the format for Wolfram Alpha, namely {{a,b..},..}
  :ARRAY   - Prints it as a common lisp 2D array.

If the STREAM is NIL, a string of the output is returned. Otherwise the
matrix itself is returned."))

;; toolkit.lisp
(docs:define-docs
  (variable *float-type*
    "The concrete float type to use in this library. 

This must be the same as 3d-vectors::*float-type*.")

  (variable *matrix-limit*
    "The maximum extent of a matrix dimension.

This is set to either the square root of ARRAY-DIMENSION-LIMIT
or the square root of MOST-POSITIVE-FIXNUM, whichever is smaller.")

  (variable *eps*
    "The allowed divergence for ~= and m~= to succeed.")

  (type mat-dim
    "Concrete type for the allowed matrix dimension size.")

  (type float-type
    "Concrete type for the used floating point type.")

  (function define-ofun
    "Same as DEFUN but with optimisation flags set.

The flags will be set accordingly:
SPEED 3, SPACE 3, DEBUG 0, SAFETY 1, COMPILATION-SPEED 0")

  (function ensure-float
    "Takes a real and returns a float of the appropriate *FLOAT-TYPE*.")

  (function ensure-float-param
    "The same as ENSURE-FLOAT, but to be used in macros for compile-time expansion.")

  (function ensure-function
    "If the argument is a symbol, it resolves it to a function by FDEFINITION.

Otherwise it can only be a function in which case it is returned verbatim.")

  (function ~=
    "Does \"approximate comparison\" by testing whether the two numbers are within *EPS* from each other."))
