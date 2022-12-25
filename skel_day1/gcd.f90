module mod_gcd
  implicit none

contains

  pure function gcd(a, b) result(g)
    integer, intent(in) :: a, b
    integer :: g  
    integer :: r
    integer :: ia, ib

    if (abs(a) > abs(b)) then
      ia = abs(a)
      ib = abs(b)
    else
      ia = abs(b)
      ib = abs(a)
    end if

    if (ib == 0) then
      if (ia == 0) then
        g = 0
      else
        g = ia
      end if
    else
      do 
        r = mod(ia, ib)
        if (r == 0) exit
        ia = ib
        ib = r
      end do
      g = ib
    end if
  end function gcd
end module mod_gcd