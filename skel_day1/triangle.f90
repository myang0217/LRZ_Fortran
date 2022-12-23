program triangle
  implicit none
  integer, parameter :: ndim = 4
  integer :: i
  real :: a(ndim), b(ndim), c(ndim), area(ndim)
  logical :: d(ndim)
  real :: s ! perimeter
  logical, external :: tri_exist
  real, external :: tri_area
  intrinsic :: sqrt
! 
! input values
  a(1) = 2.0; b(1) = 3.0; c(1) = 4.0
  a(2) = 1.0; b(2) = 1.0; c(2) = 5.0
  a(3) = 1.0; b(3) = 1.000005; c(3) = 0.000006
  a(4) = -2.0; b(4) = 3.0; c(4) = 4.0
!
! Please add code for calculation below
!

  do i = 1, ndim
    if (.not. tri_exist(a(i), b(i), c(i))) then
    else
      area(i) = tri_area(a(i), b(i), c(i))
    end if
  end do

end program triangle

logical function tri_exist(a, b, c)
  implicit none
  real, intent(in) :: a, b, c

  if (a + b > c .and. a + c > b .and. b + c > a) then
    tri_exist = .true.
    return
  else
    tri_exist = .false.
    print *, "Triangle does not exist"
  end if
  
end function tri_exist

real function tri_area(a, b, c)
  implicit none
  real, intent(in) :: a, b, c
  real :: s

  s = (a + b + c) / 2.0
  tri_area = sqrt(s * (s - a) * (s - b) * (s - c))
  print *, "Area of triangle is ", tri_area
  
end function tri_area
