program benchmark

   use kinds
   use forsvd, only: svd
   use fortime, only: timer

   implicit none

   real(rk), dimension(:, :), allocatable :: A, U, VT
   real(rk), dimension(:),    allocatable :: S
   integer                                :: m, n, i, ntests
   type(timer)                            :: t

   m = 1000
   n = 1000

   allocate(A(m,n), U(m,m), S(min(m,n)), VT(n,n))

   call random_number(A)
   A = A*10.0_rk


   ntests = 5

   call t%timer_start()
   do i = 1, ntests
      call svd(A, U,S,VT, 'gesvd')
   end do
   call t%timer_stop(nloops = ntests, message='Elapsed time (gesvd): ')


   call t%timer_start()
   do i = 1, ntests
      call svd(A, U,S,VT, 'gesdd')
   end do
   call t%timer_stop(nloops = ntests, message='Elapsed time (gesdd): ')


   deallocate(A, U, S, VT)

end program benchmark
