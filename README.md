[![GitHub](https://img.shields.io/badge/GitHub-ForSVD-blue.svg?style=social&logo=github)](https://github.com/gha3mi/forsvd)
[![Version](https://img.shields.io/github/release/gha3mi/forsvd.svg)](https://github.com/gha3mi/forsvd/releases/latest)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/forsvd/)
[![License](https://img.shields.io/github/license/gha3mi/forsvd?color=green)](https://github.com/gha3mi/forsvd/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/forsvd/actions/workflows/ci.yml/badge.svg)](https://github.com/gha3mi/forsvd/actions/workflows/ci.yml)

<img alt="ForSVD" src="https://github.com/gha3mi/forsvd/raw/main/media/logo.png" width="750">

**ForSVD**: A Fortran library for singular value decomposition (SVD) calculation, low-rank approximation, and image compression.

## fpm dependency

If you want to use `ForSVD` as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
forsvd = {git="https://github.com/gha3mi/forsvd.git"}
```

## How to run tests

**Reuirements:**

Fortran Compiler, LAPACK or MKL Libraries

**Clone the repository:**

You can clone the `ForSVD` repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/forsvd.git
```

```shell
cd forsvd
```

**Intel Fortran Compiler (ifort)**

```shell
fpm @ifort-test
```
**Intel Fortran Compiler (ifx)**

```shell
fpm @ifx-test
```

**GNU Fortran Compiler (gfortran)**

```shell
fpm @gfortran-test
```

**NVIDIA Compiler (nvfortran)**

```shell
fpm @nvfortran-test
```

## Usage (SVD)

```Fortran
use forsvd, only : svd

call svd(A, U,S,VT, method='gesvd') ! method='gesdd' 
```

### Example 1

```fortran
program example1

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

end program example1
```

## Usage (low-rank approximation)

```Fortran
use forsvd, only : tsvd

call ts%lowrank(matrix=A, rank=n)
```

### Example 2

```fortran
program example2

   use kinds
   use forsvd, only : tsvd

   implicit none

   real(rk), dimension(:,:), allocatable :: A
   type(tsvd)                            :: ts

   allocate(A(50,20))
   call random_number(A)
   A = A*100.0_rk

   call ts%lowrank(matrix=A, rank=10)

   print*, norm2(A - ts%matrix_app)/norm2(A)
   
   call ts%dlloc()

end program example2
```

## API documentation

The most up-to-date API documentation for the master branch is available
[here](https://gha3mi.github.io/forsvd/).
To generate the API documentation for `ForSVD` using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing

Contributions to `ForSVD` are welcome!
If you find any issues or would like to suggest improvements, please open an issue.
