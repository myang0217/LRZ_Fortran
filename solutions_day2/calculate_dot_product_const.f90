program calculate_dot_product
  use blas77
  implicit none
! NOTE: some compilers run into trouble with zero-sized arrays
! appearing in the array constructor below.
! A workaround for this is possible, though.
  integer, parameter :: nd = 100, pattern(4) = [1,2,1,-2]
  integer, parameter :: irep = nd/size(pattern(:))
  integer, parameter :: rest = mod(nd,size(pattern(:)))
  integer :: i
  real, dimension(nd) :: y = [ (pattern(:),i=1,irep),pattern(:rest)]
  real, dimension(nd) :: x = 1.0
  real :: dot_result

  dot_result = sdot(nd, x, 1, y, 1)

  write(*, fmt='(A, E14.7)') 'Dot product has value ', dot_result
end program
