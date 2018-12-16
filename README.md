## About 3d-matrices
This is a library implementing common matrix operations, mainly intended as the counterpiece to [3d-vectors](https://shinmera.github.io/3d-vectors) and thus being aimed at operations in 3D space. Still, it also implements other common matrix tasks such as LU and QR factorisation, determinant computation, and sub-matrix selection. 2x2, 3x3, and 4x4 matrices are specially treated and often have specifically optimised or inlined variants in the operations to ensure as high a speed as possible. NxM matrices are also available, but will always use a general algorithm in the operations.

This library by no means attempts, nor comes in any way close to replacing or imitating things such as BLAS and LIN/LAPACK. The main purpose is to have a library that allows convenient matrix operations in conjunction with the 3d-vectors library. It should be sufficiently fast and accurate for most purposes, but should not be used for serious matrix based calculations. Please use industry-standard packages for that.

## How To
Load it through ASDF or Quicklisp and use the package.

    (ql:quickload :3d-matrices)
    (use-package :3d-matrices)

All the functions are prefixed with an `m` or with `nm` for destructive ops. This should ensure that there are no clashes in names. Now let's look at creating matrices.

    (mat 1 2 3 4)
    (mat2 1)
    (mat2 '(1 2 3 4))
    (mcopy (mat3))
    (matn 2 3)
    (meye 5)
    (muniform 2 3 1)
    (mrand 10 10)

In order to see the matrix in a more human-readable format, you can use `describe` or `write-matrix` directly:

    (describe (meye 5))
    (write-matrix (mat2) T)
    (write-matrix (mat2) T :format :wolfram)
    (write-matrix (mat2) T :format :array)
    
Matrices always use `float`s. Where sensible, operations will accept `real`s as well however. Either `single-float`s or `double-float`s are used, depending on the presence of the `:3d-vectors-double-floats` keyword in `*features*`. This feature is taken over from the 3d-vectors library to ensure that both of them always agree on the float type.

The type `mat` includes all subtypes `mat2`, `mat3`, `mat4`, and `matn`. Each of them have their own specific accessors that are suffixed with the dimension number. Usually you should be fine with using the generic variants, but if you already know the type you should probably fall back to using the specific one, or use `with-fast-matref`.

    (miref (meye 2) 3)
    (mcref (meye 2) 1 1)
    (with-fast-matref (e (mat 2) 2)
      (e 1 1))

Matrices are basically a struct that contains a simple-vector of floats. This means that every single reference must also dereference the array first. This is why, if you have many repeated accesses to an array, you should use `with-fast-matref` or do the same manually by first retrieving the backing array with `marr`.

If you're coming to this library with the intention of using it to do 3D math, you'll most likely be mostly looking for how to create translation, rotation, and scaling matrices. Specific functions exist for this that take care of it for you. They all operate on `mat4`s and take a `vec3` as argument.

    (let ((mat (mtranslation (vec 1 2 3))))
      (nmscale mat (vec 1 2 3))
      (nmrotate mat +vx+ 90)
      (m* mat (vec 1 2 3 4)))

Aside from translations you'll probably also want to set up a projection and a camera. You can do this, too.

    (mperspective 75 (/ w h) 0.001 10000)       ; Perspective projection
    (mortho 0 w h 0 0.001 10000)                ; Orthographic projection
    (nmlookat modelview camera-pos center +vy+) ; Look at the centre

Aside from the basic comparison operators `m=` `m~=` `m/=` `m<` `m>` `m<=` `m>=`, and arithmetic operators `m+` `m-` `m*` `m/` `nm+` `nm-` `nm*` `n*m` `nm/`, 3d-matrices also includes LU decomposition `mlu`, determinant computation `mdet`, inversion `minv`, transposition `mtranspose`, trace calculation `mtrace`, minors `mminor`, cofactors `mcof` `mcofactor`, matrix adjugate `madj`, pivoting `mpivot`, norming `m1norm` `minorm` `m2norm`, QR decomposition `mqr` and eigenvalue calculation `meigen`. These should all work "as you expect" and I will thus refrain from showing them off in detail here. Refer to your standard linear algebra textbook to get an understanding of what they do if you don't know already.

Finally, There's also some basic operators to do sectioning or restructuring of a matrix.

    (mcol (mat 1 2 3 4) 0)
    (mrow (mat 1 2 3 4) 0)
    (mdiag (mat 1 0 0 0 2 0 0 0 3))
    (mtop (mat 1 2 3 4 5 6 7 8 9) 2)
    (nmswap-row (mat 1 2 3 4) 0 1)

And that's pretty much all she wrote. Note that some operations will only work on square or non-singular matrices, and all operations that take multiple operands require them to be of a compatible type. For example, you can only multiply matrices that are of agreeable rows and columns or multiply with a vector that is of the appropriate size.

## Also See

* [3d-vectors](https://shinmera.github.io/3d-vectors)
