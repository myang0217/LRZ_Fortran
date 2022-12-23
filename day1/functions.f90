module mod_functions
  implicit none
contains
! implementation of a module function
  real function wsqrt(x, p)
    real, intent(in) :: x, p
    if (abs(x) <= abs(p)) then
       wsqrt = sqrt(1. - (x/p)**2)
    else
       wsqrt = 0.0
    end if
  end function wsqrt
end module mod_functions
program functions
  use mod_functions
  implicit none
  real :: x1, x2, p, y
  x1 = 3.2; x2 = 2.1; p = 4.7

  y = wsqrt(x1,p) + wsqrt(x2,p)**2
  write(*,*) 'y = ',y
  if (wsqrt(3.1,p) < 0.3) then
     write(*,*) 'smaller'
  else
     write(*,*) 'greater or equal'
  end if
end program functions
