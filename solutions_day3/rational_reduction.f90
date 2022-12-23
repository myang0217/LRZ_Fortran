module rational_reduction
  use rational
  implicit none
  interface sum
     procedure sum_rational
  end interface sum
contains
  type(fraction) function sum_rational(array)
    type(fraction), intent(in) :: array(..)
    integer :: i, j, k
    sum_rational = fraction(0,1)
    select rank (array)
    rank (1)
      do i = 1, size(array, 1)
         sum_rational = sum_rational + array(i)
      end  do
    rank (2)
      do j = 1, size(array, 2)
         do i = 1, size(array, 1)
            sum_rational = sum_rational + array(i,j)
         end  do
      end do
    rank (3)
      do k = 1, size(array, 3)
         do j = 1, size(array, 2)
            do i = 1, size(array, 1)
               sum_rational = sum_rational + array(i,j,k)
            end  do
         end do
      end do
    rank default
      stop 'only ranks 1,2,3 supported.'
    end select
  end function sum_rational
end module rational_reduction
