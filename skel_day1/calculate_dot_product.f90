program calculate_dot_product
  use blas77
  implicit none
  integer, parameter :: nd = 100
  real, dimension(nd) :: x, y
  real :: dot_result
  integer :: i

  do i=1, nd
    x(i) = 1.0
    select case ( ??? )
!   FIXME: add statements that initialize y as indicated in exercise sheet
    end select
  end do

!   FIXME: complete the assignment statement
  dot_result = 

  write(*, fmt='(A, E14.7)') 'Dot product has value ', dot_result
end program
