module mod_solver
  implicit none
contains
  subroutine solve_quadratic(a, b, c, n, x1, x2)
    !
    ! a single precision procedure to solve a real quadratic 
    ! equation a * x**2 + b * x + c = 0
    implicit none
    real, intent(in) :: a, b, c     ! coefficients
    integer, intent(out) :: n       ! number of solutions
                                    ! -1 indicates an infinity of them
    real, intent(inout) :: x1, x2   ! solutions in ascending order

    ! Note that this is still not perfect because the calculation 
    ! of the discriminant can overflow ...
    real :: disc, r, tmp

    if (a == 0.0) then
       if (b == 0.0) then
          n = 0   
          if (c == 0.0) n = -1
          return
       else
          n = 1
          x1 = - c / b
       end if
       return
    end if

    disc = b**2 - 4 * a * c
    if (disc > 0.0) then
       n = 2
       if (b == 0.0) then
          r = abs(sqrt(disc) / (2.0 * a))
          x1 = -r
          x2 = r
       else
          tmp = -0.5 * (b + sign(sqrt(disc),b))
          x1 = tmp / a
          x2 = c / tmp
          if (x1 > x2) then
             tmp = x1
             x1 = x2
             x2 = tmp
          end if
       end if
    elseif (disc == 0.0) then
       n = 1
       x1 = -0.5 * b / a
    else
       n = 0
    end if
  end subroutine solve_quadratic
  subroutine solve_quadratic_simple(a, b, c, n, x1, x2)
    !
    ! a single precision procedure to solve a real quadratic 
    ! equation a * x**2 + b * x + c = 0
    implicit none
    real, intent(in) :: a, b, c     ! coefficients
    integer, intent(out) :: n       ! number of solutions
                                    ! -1 indicates an infinity of them
    real, intent(inout) :: x1, x2   ! solutions in ascending order
    ! simple-minded version that can fail pretty badly
    ! under some circumstances
    real :: disc, tmp

    disc = b**2 - 4 * a * c
    if (disc > 0.0) then
       n = 2
       x1 = ( -b + sqrt(disc) ) / ( 2. * a )
       x2 = ( -b - sqrt(disc) ) / ( 2. * a )
       if (x1 > x2) then
          tmp = x1
          x1 = x2
          x2 = tmp
       end if
    elseif (disc == 0.0) then
       n = 1
       x1 = -0.5 * b / a
    else
       n = 0
    end if
  end subroutine solve_quadratic_simple
end module mod_solver
