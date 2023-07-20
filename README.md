![ForSVD](media/logo.png)
============

**ForSVD**: This Fortran module provides functions and subroutines for calculating the singular value decomposition (SVD) of a matrix. It includes different methods for computing the SVD, allowing flexibility based on the requirements of the application.

-----

## Table of Contents

- [](#)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
    - [fpm](#fpm)
  - [Usage](#usage)
  - [Subroutine: `svd`](#subroutine-svd)
    - [Inputs](#inputs)
    - [Outputs](#outputs)
    - [Optional Input](#optional-input)
  - [Methods](#methods)
  - [External Subroutine: `dgesvd`](#external-subroutine-dgesvd)
  - [External Subroutine: `dgesdd`](#external-subroutine-dgesdd)
  - [Tests](#tests)
  - [Example](#example)
  - [Documentation](#documentation)
  - [Contributing](#contributing)
-----

## Installation

### fpm
ForSVD can be cloned and then built using [fpm](https://github.com/fortran-lang/fpm), following the instructions provided in the documentation available on Fortran Package Manager.

```bash
git clone https://github.com/gha3mi/forsvd.git
cd forsvd
fpm install --prefix .
```

Or you can easily include this package as a dependency in your `fpm.toml` file.

```toml
[dependencies]
forsvd = {git="https://github.com/gha3mi/forsvd.git"}
```

-----
## Usage
1. Include the `ForSVD` module in your Fortran program.
2. Declare the necessary variables and arrays.
3. Call the `svd` subroutine with the appropriate input arguments to compute the SVD.
4. The resulting left singular vectors, right singular vectors, and singular values will be stored in the corresponding output arrays.
-----

## Subroutine: `svd`
Calculates the singular value decomposition (SVD) of a matrix A using the specified method.

### Inputs
- `A`: Input matrix A.

### Outputs
- `U`: Left singular vectors.
- `S`: Singular values.
- `VT`: Right singular vectors.

### Optional Input
- `method`: Specifies the method to be used for computing the SVD. If not provided, the default method is used.

## Methods
1. `gesvd` (default): Computes the SVD using the `dgesvd` external subroutine.
2. `gesdd`: Computes the SVD using the `dgesdd` external subroutine.

**Note:** The external subroutines `dgesvd` and `dgesdd` require the `kinds` module.

## External Subroutine: `dgesvd`
Computes the SVD of a matrix using the LAPACK routine `dgesvd`.

## External Subroutine: `dgesdd`
Computes the SVD of a matrix using the LAPACK routine `dgesdd`.

-----

## Tests

The `tests` directory contains test programs to verify the functionality of the `fortime` module. To run the tests using `fpm`, you can use response files for specific compilers:

- For Intel Fortran Compiler (ifort):
```bash
fpm @ifort
```

- For Intel Fortran Compiler (ifx):
```bash
fpm @ifx
```

- For NVIDIA Compiler (nvfortran):
```bash
fpm @nvidia
```

- For GNU Fortran Compiler (gfortran):
```bash
fpm @gfortran
```
-----

## Example
```fortran
program test1

   use :: kinds
   use :: forsvd, only : svd

   implicit none

   real(rk), dimension(:, :), allocatable :: A, U, VT
   real(rk), dimension(:),    allocatable :: S
   integer                                :: m, n, i, j

   m = 4
   n = 3

   allocate(A(m,n), U(m,m), S(min(m,n)), VT(n,n))

   call random_number(A)
   A = A*10.0_rk

   call svd(A, U,S,VT)

   ! Print U
   print *, "U:"
   print "(4F10.6)", (U(:,j), j = 1, m)

   ! Print S
   print *, "S:"
   print "(3F10.6)", S

   ! Print VT
   print *, "VT:"
   print "(3F10.6)", (VT(:,j), j = 1, n)

   deallocate(A, U, S, VT)

end program test1
```
-----

## Documentation
To generate the documentation for the `fortime` module using [ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following command:
```bash
ford project.yml
```

-----

## Contributing

Contributions to fortime are welcome! If you find any issues or would like to suggest improvements, please open an issue or submit a pull request.