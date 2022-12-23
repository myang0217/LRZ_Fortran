module rational
!
! module for rational numbers
! Note: this is far from perfect. No proper
! treatment of Inf/NaN values is done.
! Also, no comparison operators are implemented 
!  
  implicit none
  private
  integer, parameter :: ik = kind(1), lk = selected_int_kind(10)
  public :: assignment(=), operator(+), operator(-), &
       operator(*), operator(/), operator(.inv.), write
  type, public :: fraction
     private
     integer(kind=lk) :: numer
     integer(kind=lk) :: denom
  end type fraction
!
! Overloading or extending the structure constructor is a Fortran 2003
! feature that is provided as a demo here ... to avoid this, you 
! can simply use a different generic name.
  interface fraction
     module procedure frac_default
     module procedure frac
  end interface fraction
  interface assignment (=)
     module procedure assign_from_int
     module procedure assign_to_int
     module procedure assign_to_real
  end interface
  interface operator (+)
     module procedure add_frac
  end interface
  interface operator (-)
     module procedure sub_frac
  end interface
  interface operator (*)
     module procedure mult_frac
  end interface
  interface operator (/)
     module procedure div_frac
  end interface
  interface operator (.inv.)
     module procedure invert
  end interface
contains
  elemental integer function gcd(i, j)
    integer(lk), intent(in) :: i, j
    !
    integer(lk) :: ia, ib, id, ir
    !
    ia = i; if (i < 0_lk) ia = - i
    ib = j; if (j < 0_lk) ib = - j

    if (ia < ib) then
       ir = ia
       ia = ib
       ib = ir
    end if

    if (ib == 0_lk) then
       ib = ia
    else
       do
          id = ia / ib
          ir = ia - id * ib  ! alternative: intrinsic ir = mod(ia, ib)
          if (ir == 0_lk) exit
          ia = ib
          ib = ir
       end do
    end if

    gcd = ib
  end function gcd
  type(fraction) elemental function frac(numer, denom)
    integer(kind=lk), intent(in) :: numer, denom
    frac%numer = numer
    frac%denom = denom
    call normalize_frac(frac)
  end function frac
  type(fraction) elemental function frac_default(numer, denom)
    integer(kind=ik), intent(in) :: numer, denom
    frac_default%numer = int(numer,lk)
    frac_default%denom = int(denom,lk)
    call normalize_frac(frac_default)
  end function frac_default
  subroutine assign_to_int(r, x)
    type(fraction), intent(in) :: x
    integer(kind=lk), intent(out) :: r(2)
    r(1) = x%numer
    r(2) = x%denom
  end subroutine assign_to_int
  subroutine assign_from_int(r, x)
    integer, intent(in) :: x
    type(fraction), intent(out) :: r 
    r%numer = x
    r%denom = 1
  end subroutine assign_from_int
  subroutine assign_to_real(r, x)
    type(fraction), intent(in) :: x
    real, intent(out) :: r
    r = real(x%numer)/real(x%denom)
  end subroutine assign_to_real
  elemental function add_frac(x,y) result (r)
    type(fraction), intent(in) :: x, y
    type(fraction) :: r
!
    r%numer = x%numer * y%denom + y%numer * x%denom
    r%denom = x%denom * y%denom
    call normalize_frac(r)
  end function add_frac
  elemental function sub_frac(x,y) result (r)
    type(fraction), intent(in) :: x, y
    type(fraction) :: r
!
    r%numer = x%numer * y%denom - y%numer * x%denom
    r%denom = x%denom * y%denom
    call normalize_frac(r)
  end function sub_frac
  elemental function mult_frac(x,y) result (r)
    type(fraction), intent(in) :: x, y
    type(fraction) :: r
!
    r%numer = x%numer * y%numer
    r%denom = x%denom * y%denom
    call normalize_frac(r)
  end function mult_frac
  elemental function div_frac(x,y) result (r)
    type(fraction), intent(in) :: x, y
    type(fraction) :: r
!
    r%numer = x%numer * y%denom
    r%denom = x%denom * y%numer
    call normalize_frac(r)
  end function div_frac
  elemental function invert(x) result (r)
    type(fraction), intent(in) :: x
    type(fraction) :: r
    r%numer = x%denom
    r%denom = x%numer
  end function invert
  subroutine write(r)
    type(fraction), intent(in) :: r
    write(*, fmt='(''('',I0,''/'',I0,'')'')') r
  end subroutine write
  elemental subroutine normalize_frac(r)
    type(fraction), intent(inout) :: r
    integer(kind=lk) :: g
    g = gcd(r%numer,r%denom)
    r%numer = r%numer / g
    r%denom = r%denom / g
    if (r%denom < 0) then
       r%denom = - r%denom
       r%numer = - r%numer
    else if (r%denom == 0) then
       r%numer = r%numer / abs(r%numer)
    end if
  end subroutine normalize_frac
end module rational
