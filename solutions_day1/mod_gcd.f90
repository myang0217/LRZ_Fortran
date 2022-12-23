module mod_gcd
  implicit none
  intrinsic :: mod
contains
  pure integer function gcd(i, j)

    integer, intent(in) :: i, j
    ! proper intent specification avoids undesirable side effects

    integer :: ia, ib, id, ir
    !
    ia = i; if (i < 0) ia = - i
    ib = j; if (j < 0) ib = - j

    if (ia < ib) then
       ir = ia
       ia = ib
       ib = ir
    end if

    if (ib == 0) then
       ib = ia
    else
       do
          id = ia / ib
          ir = ia - id * ib  ! alternative: intrinsic ir = mod(ia, ib)
          if (ir == 0) exit
          ia = ib
          ib = ir
       end do
    end if

    gcd = ib
  end function gcd
!
! alternative solution
! that uses a recursive function internally
  pure integer function gcd_r(i, j)
    integer, intent(in) :: i, j
    integer ::  ia, ib, ir
!
    ia = i; if (i < 0) ia = - i
    ib = j; if (j < 0) ib = - j

    if (ia < ib) then
       ir = ia
       ia = ib
       ib = ir
    end if

    gcd_r = g(ia, ib)
  contains
    pure recursive function g(i, j) result(r)
      integer, intent(in) :: i, j
      integer :: r
      if (j == 0) then
         r = i
      else
         r = g(j, mod(i, j))
      end if
    end function g
  end function gcd_r
end module mod_gcd
