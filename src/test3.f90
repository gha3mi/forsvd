program test3

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

   call svd(A, U,S,VT, 'gesdd')

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

end program test3


