program SumOfSquares
  implicit none
  integer, parameter :: sp = selected_int_kind(7)
  integer(sp), parameter :: up = 1000_sp
  integer(sp) :: sum
  integer(sp), parameter :: dim = 50_sp
  integer(sp) :: j
  integer(sp), dimension(dim) :: jArray

  sum = 0
  jArray = 0

   do j = 1, dim
     jArray(j) = j * j
     sum = sum + jArray(j)

     if (sum < up) then
       write(*,'(a,I10)') 'sum of the first j terms of the square series is ', sum
     else
       exit
     end if
  end do

 end program SumOfSquares

