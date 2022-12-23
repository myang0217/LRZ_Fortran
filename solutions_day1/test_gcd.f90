program test_gcd
  use mod_gcd
  implicit none
  call check_function(12, 2, 2)
  call check_function(-12, 3, 3)
  call check_function(-12, -3, 3)
  call check_function(12, -3, 3)
  call check_function(-3, 12, 3)
  call check_function(12, 8, 4)
  call check_function(12, -8, 4)
  call check_function(0, 4, 4)
  call check_function(0, -4, 4)
  call check_function(0, 0, 0)
  call check_function(42, 49, 7)
contains
  subroutine check_function(i,j,k)
    integer, intent(in) :: i, j, k
    integer :: l
    l = gcd(i, j)
!    l = gcd_r(i, j)
    if (l == k) then
       write(*, *) 'OK'
    else
       write(*, *) 'FAIL:   GCD(',i,j,' should be ',k,' but is ',l
    end if
  end subroutine check_function
end program test_gcd
