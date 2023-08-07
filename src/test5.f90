program test5

   use forsvd, only : pixel

   implicit none

   type(pixel) :: px

   px%nrow = 356
   px%ncol = 238

   call px%image_to_pixels(image_name='pixel/John_W_Backus.jpg', file_name='pixel/John_W_Backus.txt')
   call px%load_pixels(file_name='pixel/John_W_Backus.txt')
   call px%compress_pixels(rank=10)
   call px%save_pixels(file_name='pixel/John_W_Backus_com.txt')
   call px%pixels_to_image(image_name='pixel/John_W_Backus.jpg', file_name='pixel/John_W_Backus.txt')
   call px%dlloc()

end program test5
