program test4

   use kinds
   use forsvd, only: tsvd

   implicit none

   real(rk), dimension(:,:), allocatable :: A
   type(tsvd)                            :: ts

   allocate(A(50,20))
   call random_number(A)
   A = A*100.0_rk

   call ts%lowrank(matrix=A, rank=10)

   print*, norm2(A - ts%matrix_app)/norm2(A)
   
   call ts%dlloc()

end program test4
