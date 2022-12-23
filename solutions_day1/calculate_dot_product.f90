program calculate_dot_product
  use blas77
  implicit none
  integer, parameter :: nd = 100
  real, dimension(nd) :: x, y
  real :: dot_result
  integer :: i

  do i=1, nd
    x(i) = 1.0
    select case (mod(i, 4))
    case (0) 
      y(i) = -2.0
    case (2) 
      y(i) = 2.0
    case default
      y(i) = 1.0
    end select
  end do

  dot_result = sdot(nd, x, 1, y, 1)

  write(*, fmt='(A, E14.7)') 'Dot product has value ', dot_result
end program
