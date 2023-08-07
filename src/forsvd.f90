module forsvd

   ! This module provides functions and subroutines
   ! for calculating the singular value decomposition (SVD).

   use :: kinds

   implicit none

   private

   public :: svd, tsvd, pixel

   !===============================================================================
   type :: tsvd
      real(rk), dimension(:,:), allocatable :: matrix
      real(rk), dimension(:,:), allocatable :: matrix_app
      integer                               :: nrow, ncol
      integer                               :: rank
   contains
      procedure :: lowrank
      procedure :: dlloc => deallocate_tsvd
   end type tsvd
   !===============================================================================


   !===============================================================================
   type :: pixel
      character(256)                        :: image_name
      character(256)                        :: file_name
      real(rk), dimension(:,:), allocatable :: pixels
      real(rk), dimension(:,:), allocatable :: pixels_app
      integer                               :: nrow, ncol
      integer                               :: rank
   contains
      procedure :: image_to_pixels ! TODO:
      procedure :: load_pixels
      procedure :: compress_pixels
      procedure :: save_pixels
      procedure :: pixels_to_image  ! TODO:
      procedure :: dlloc => deallocate_pixel
   end type pixel
   !===============================================================================


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
      real(rk), dimension(:, :), contiguous, intent(in)  :: A    ! Input matrix A

      ! Outputs:
      real(rk), dimension(:,:), intent(out) :: U    ! Left singular vectors
      real(rk), dimension(:,:), intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(:),   intent(out) :: S    ! Singular values

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
      real(rk), dimension(:,:), intent(out) :: U    ! Left singular vectors
      real(rk), dimension(:,:), intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(:),   intent(out) :: S    ! Singular values

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
      real(rk), dimension(:, :), contiguous,          intent(in)  :: A    ! Input matrix A

      ! Outputs:
      real(rk), dimension(:,:), intent(out) :: U    ! Left singular vectors
      real(rk), dimension(:,:), intent(out) :: VT   ! Right singular vectors
      real(rk), dimension(:),   intent(out) :: S    ! Singular values

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


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine lowrank(this, matrix, rank, method)
      class(tsvd),  intent(inout)            :: this
      real(rk), dimension(:, :), intent(in)  :: matrix
      integer,      intent(in)               :: rank
      character(*), intent(in), optional     :: method
      real(rk), dimension(:, :), allocatable :: U, VT
      real(rk), dimension(:),    allocatable :: S
      integer                                :: i, j, irank

      this%matrix = matrix
      this%nrow   = size(matrix,1)
      this%ncol   = size(matrix,2)
      this%rank   = rank

      allocate(U(this%nrow,this%nrow), S(min(this%nrow,this%ncol)), VT(this%ncol,this%ncol))

      call svd(this%matrix, U,S,VT, method)

      allocate(this%matrix_app(this%nrow,this%ncol))
      this%matrix_app = 0.0_rk
      do irank = 1, rank
         do j = 1,this%ncol
            do i = 1,this%nrow
               this%matrix_app(i,j) = this%matrix_app(i,j) + U(i, irank)*S(irank)*VT(irank, j)
            end do
         end do
      end do

   end subroutine lowrank
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   ! TODO:
   subroutine image_to_pixels(this, image_name, file_name)
      class(pixel), intent(inout) :: this
      character(*), intent(in)    :: image_name
      character(*), intent(in)    :: file_name
      integer :: i, nunit

      call execute_command_line('python pixel/image_to_pixels.py')

   end subroutine image_to_pixels
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   ! TODO:
   subroutine pixels_to_image(this, file_name, image_name)
      class(pixel), intent(inout) :: this
      character(*), intent(in)    :: file_name
      character(*), intent(in)    :: image_name
      integer :: i, nunit

      call execute_command_line('python pixel/pixels_to_image.py')

   end subroutine pixels_to_image
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_pixels(this, file_name)
      class(pixel), intent(inout) :: this
      character(*), intent(in) :: file_name
      integer :: i, nunit

      allocate(this%pixels(this%nrow,this%ncol))

      open(newunit=nunit, file=trim(file_name), status='old')
      do i = 1, this%nrow
         read(nunit, *) this%pixels(i, :)
      end do
      close(nunit)

   end subroutine load_pixels
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine compress_pixels(this, rank, method)
      class(pixel), intent(inout)            :: this
      integer,      intent(in)               :: rank
      character(*), intent(in), optional     :: method
      type(tsvd)                             :: mat

      call mat%lowrank(this%pixels, rank, method)
      this%pixels_app = mat%matrix_app

   end subroutine compress_pixels
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine save_pixels(this, file_name)
      class(pixel), intent(inout) :: this
      character(*), intent(in) :: file_name
      integer :: i, nunit

      open(newunit=nunit, file=trim(file_name))
      do i = 1, this%nrow
         write(nunit, *) this%pixels_app(i, :)
      end do
      close(nunit)

   end subroutine save_pixels
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_pixel(this)
      class(pixel), intent(inout) :: this
      if (allocated(this%pixels)) deallocate(this%pixels)
      if (allocated(this%pixels_app)) deallocate(this%pixels_app)
   end subroutine deallocate_pixel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_tsvd(this)
      class(tsvd), intent(inout) :: this
      if (allocated(this%matrix)) deallocate(this%matrix)
      if (allocated(this%matrix_app)) deallocate(this%matrix_app)
   end subroutine deallocate_tsvd
   !===============================================================================

end module forsvd
