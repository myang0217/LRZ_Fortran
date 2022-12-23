program solve_my_quadratic 
  implicit none
  real, parameter :: a = 2.0, b = -2.0, c = -1.5
  real :: x1, x2
  intrinsic :: sqrt

  x1 = ( -b + sqrt(b**2 - 4. * a * c) ) / ( 2. * a )
  x2 = ( -b - sqrt(b**2 - 4. * a * c) ) / ( 2. * a )

  write(*, fmt=*) 'Solutions are: ', x1, x2
end program

