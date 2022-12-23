program test_quadratic
  use mod_solver
  implicit none
  real :: a1, a2, a3, x, y
  integer :: nsol

!   a1 = 2.0; a2 = -2.0; a3 = -1.5
   a1 = 2.0; a2 = 7.4; a3 = 0.2
!   a1 = 0.0; a2 = 7.4; a3 = 0.2
!   a1 = 2.0; a2 = 7.4; a3 = 0.2e-4

  call solve_quadratic(a1, a2, a3, nsol, x, y)
!  call solve_quadratic_simple(a1, a2, a3, nsol, x, y)

  write(*,*)  'Number of solutions', nsol

  select case (nsol)
  case (-1)
     write(*, *) 'Infinitely many solutions found.'
  case (0)
     write(*, *) 'Zero solutions found.'
  case (1)
     write(*, *) 'One solution found: ', x
     write(*, *) 'Check solution: ', q_eval(x)
  case (2)
     write(*, *) 'Two solutions found:', x, y
     write(*, *) 'Check solution 1: ', q_eval(x)
     write(*, *) 'Check solution 2: ', q_eval(y)
  end select
contains
  real function q_eval(x)
    real, intent(in) :: x
    q_eval = sign(1.0, x)*(a1*x + a2) + a3/abs(x)
  end function q_eval
end program test_quadratic
