program triangle
  implicit none
  integer, parameter :: ndim = 4, iopt = 1
  integer :: i
  real :: a(ndim), b(ndim), c(ndim), area(ndim)
  real :: s
  intrinsic :: sqrt
! 
! input values
  a(1) = 2.0; b(1) = 3.0; c(1) = 4.0
  a(2) = 1.0; b(2) = 1.0; c(2) = 5.0
  a(3) = 1.0; b(3) = 1.000005; c(3) = 0.000006
  a(4) = -2.0; b(4) = 3.0; c(4) = 4.0
  list : do i = 1, ndim
    s = (a(i)+b(i)+c(i))/2.0
    if (s < 0.0 .or. s-a(i) < 0.0 .or. s-b(i) < 0.0 .or. s-c(i) < 0.0) then
      write(*,*) ' Skipping invalid input data. '
      cycle list
    end if
    select case(iopt)
    case(1)        ! Heron's formula
       area(i) = sqrt(s*(s-a(i))*(s-b(i))*(s-c(i)))
    case(2)        ! Alternative, numerically more stable
       area(i) = ((a(i)+(b(i)+c(i)))*(c(i)-(a(i)-b(i)))*(c(i)+ &
                  (a(i)-b(i)))*(a(i)+(b(i)-c(i))))**0.5/4.0
    case default
       stop 'triangle: value of iopt not implemented'
    end select
    write(*, *) ' Area No. ',i, ' is ', area(i)
  end do list
 
end program triangle
