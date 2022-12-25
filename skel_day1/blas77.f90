module blas77
  implicit none
  interface
  real function sdot(n, sx, incx, sy, incy)
    integer, intent(in) :: n
    real, intent(in) :: sx(*)
    integer, intent(in) :: incx
    real, intent(in) :: sy(*)
    integer, intent(in) :: incy
  end function
  end interface
end module
