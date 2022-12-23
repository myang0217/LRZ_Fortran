program test_rational_reduction
  use rational_reduction
  implicit none

  type(fraction) :: x(3), y(2,2), xs, ys

  x(1) = fraction(1,2)
  x(2) = fraction(2,5)
  x(3) = fraction(5,12)

  y(1,1) = x(1)
  y(2,1) = x(2)
  y(1,2) = x(3)
  y(2,2) = fraction(0,1)

  xs = sum(x)
  ys = sum(y)

  write(*,*) 'Sum of x:'
  call write(xs)
  write(*,*) 'Sum of y:'
  call write(ys)
end program test_rational_reduction
