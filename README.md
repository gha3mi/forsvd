![ForSVD](media/logo.png)
============

**ForSVD**: This Fortran module provides functions and subroutines for calculating the singular value decomposition (SVD) of a matrix. It includes different methods for computing the SVD, allowing flexibility based on the requirements of the application.


## Table of Contents

- [](#)
  - [Table of Contents](#table-of-contents)
  - [How to Use ForSVD](#how-to-use-forsvd)
    - [Adding ForSVD as an fpm Dependency](#adding-forsvd-as-an-fpm-dependency)
    - [Installation of ForSVD Library](#installation-of-forsvd-library)
  - [Example](#example)
  - [API Documentation](#api-documentation)
  - [Contributing](#contributing)



## How to Use ForSVD

**Reuirements:** Fortran Compiler, LAPACK or MKL Libraries

### Adding ForSVD as an fpm Dependency

If you want to use ForSVD as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
forsvd = {git="https://github.com/gha3mi/forsvd.git"}
```

### Installation of ForSVD Library

To use ForSVD, follow the steps below:

- **Clone the repository:**

   You can clone the ForSVD repository from GitHub using the following command:

   ```shell
   git clone https://github.com/gha3mi/forsvd.git
   ```

   ```shell
   cd forsvd
   ```

- **Build using the Fortran Package Manager (fpm):**

   ForSVD can be built using [fpm](https://github.com/fortran-lang/fpm).
   Make sure you have fpm installed, and then execute the following command:

  **GNU Fortran Compiler (gfortran)**

   ```shell
   fpm install --prefix . --compiler gfortran --flag "-Wno-line-truncation -Ofast -march=native -llapack"
   ```

  **Intel Fortran Compiler (ifort)**

   ```shell
   fpm install --prefix . --compiler ifort --flag "-Ofast -xHost -mtune=native -qmkl=parallel"
   ```

  **Intel Fortran Compiler (ifx)**

    ```shell
   fpm install --prefix . --compiler ifx --flag "-Ofast -xHost -mtune=native -qmkl=parallel"
   ```

## Usage
1. Include the `ForSVD` module in your Fortran program.
2. Declare the necessary variables and arrays.
3. Call the `svd` subroutine with the appropriate input arguments to compute the SVD.
4. The resulting left singular vectors, right singular vectors, and singular values will be stored in the corresponding output arrays.

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

## API Documentation

To generate the API documentation for the `ForSVD` module using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing
Contributions to `ForSVD` are welcome! If you find any issues or would like to suggest improvements, please open an issue or submit a pull request.
