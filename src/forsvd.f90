module forsvd

   ! This module provides functions and subroutines
   ! for calculating the singular value decomposition (SVD).

   use :: kinds

   implicit none

   private

   public :: svd

   !===============================================================================
   interface svd
      procedure :: svd_rel ! Interface for the svd_rel subroutine
   end interface
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   !> Calculates the singular value decomposition (SVD) of a matrix A.
#if defined (PURE)
   pure subroutine svd_rel(A, U,S,VT, method)
#elif defined (IMPURE)
   impure subroutine svd_rel(A, U,S,VT, method)
#endif

      ! Inputs:
      real(rk), dimension(:, :), contiguous,          intent(in)  :: A    ! Input matrix A

      ! Outputs:
      real(rk), dimension(size(A,1), size(A,1)),      intent(out) :: U    ! Left singular vectors
      real(rk), dimension(size(A,2), size(A,2)),      intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(min(size(A,1), size(A,2))), intent(out) :: S    ! Singular values

      character(*), intent(in), optional :: method

      if (.not. present(method)) then
         call gesvd_rel(A, U,S,VT)
      else

         select case (method)
            case ('gesvd')
            call gesvd_rel(A, U,S,VT)
            case( 'gesdd')
            call gesdd_rel(A, U,S,VT)
         end select

      end if

   end subroutine svd_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   !> Calculates the singular value decomposition (SVD) of a matrix A.
#if defined (PURE)
   pure subroutine gesvd_rel(A, U,S,VT)
#elif defined (IMPURE)
   impure subroutine gesvd_rel(A, U,S,VT)
#endif

      ! Inputs:
      real(rk), dimension(:, :), contiguous,          intent(in)  :: A    ! Input matrix A

      ! Outputs:
      real(rk), dimension(size(A,1), size(A,1)),      intent(out) :: U    ! Left singular vectors
      real(rk), dimension(size(A,2), size(A,2)),      intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(min(size(A,1), size(A,2))), intent(out) :: S    ! Singular values

      ! Local variables
      real(rk)                                                    :: work1(1) ! memory allocation query
      real(rk), dimension(:), allocatable                         :: work     ! Work array
      integer                                                     :: m, n, lwork, info

      ! External subroutine for calculating the SVD
      interface dgesvd
         pure subroutine dgesvd(jobuf,jobvtf,mf,nf,af,ldaf,sf,uf,lduf,vtf,ldvtf,workf,lworkf,infof)
            use kinds
            character, intent(in)  :: jobuf, jobvtf
            integer,   intent(in)  :: mf, nf, ldaf, lduf, ldvtf, lworkf
            real(rk),  intent(in)  :: Af(ldaf, *)
            real(rk),  intent(out) :: Sf(min(mf, nf))
            real(rk),  intent(out) :: Uf(lduf, *), VTf(ldvtf, *)
            real(rk),  intent(out) :: workf(*)
            integer,   intent(out) :: infof
         end subroutine dgesvd
      end interface

      m = size(A, 1)
      n = size(A, 2)

      ! Calculate the optimal size of the work array
      call dgesvd('S', 'S', m, n, A, m, S, U, m, VT, n, work1, -1, info)
      lwork = nint(work1(1))
      allocate(work(lwork))

      call dgesvd('S', 'S', m, n, A, m, S, U, m, VT, n, work, lwork, info)

      deallocate(work)
   end subroutine gesvd_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   !> Calculates the singular value decomposition (SVD) of a matrix A.
#if defined (PURE)
   pure subroutine gesdd_rel(A, U, S, VT)
#elif defined (IMPURE)
   impure subroutine gesdd_rel(A, U, S, VT)
#endif

      ! Inputs:
      real(rk), dimension(:, :), contiguous, intent(in)  :: A    ! Input matrix A

      ! Outputs:
      real(rk), dimension(size(A,1), size(A,1)),      intent(out) :: U    ! Left singular vectors
      real(rk), dimension(size(A,2), size(A,2)),      intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(min(size(A,1), size(A,2))), intent(out) :: S    ! Singular values

      ! Local variables
      real(rk) :: work1(1) ! memory allocation query
      real(rk), dimension(:), allocatable :: work     ! Work array
      integer :: m, n, lwork, info
      integer, dimension(:), allocatable :: iwork     ! Integer work array

      ! External subroutine for calculating the SVD
      interface dgesdd
         pure subroutine dgesdd(f_jobz, f_m, f_n, f_a, f_lda, f_s, f_u, f_ldu, f_vt, f_ldvt, f_work, f_lwork, f_iwork, f_info)
            use kinds
            character,                       intent(in)    :: f_jobz
            integer,                         intent(in)    :: f_m, f_n, f_lda, f_ldu, f_ldvt, f_lwork
            real(rk),  dimension(f_lda, *),  intent(in)    :: f_a
            real(rk),  dimension(*),         intent(out)   :: f_s
            real(rk),  dimension(f_ldu, *),  intent(out)   :: f_u
            real(rk),  dimension(f_ldvt, *), intent(out)   :: f_vt
            real(rk),  dimension(*),         intent(out)   :: f_work
            integer,   dimension(*),         intent(out)   :: f_iwork
            integer,                         intent(out)   :: f_info
         end subroutine dgesdd
      end interface

      m = size(A, 1)
      n = size(A, 2)

      ! Calculate the optimal size of the work array
      call dgesdd('S', m, n, A, m, S, U, m, VT, n, work1, -1, iwork, info)
      lwork = nint(work1(1))
      allocate(work(lwork))
      allocate(iwork(n * 8)) ! Adjust the size as needed

      call dgesdd('S', m, n, A, m, S, U, m, VT, n, work, lwork, iwork, info)

      deallocate(work)
      deallocate(iwork)
   end subroutine gesdd_rel
   !===============================================================================

end module forsvd
