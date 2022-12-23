module blas77
  implicit none
  interface
    real function sdot(nx, x, incx, y, incy)
      integer, intent(in) :: nx, incx, incy
      real, intent(in) :: x(*), y(*)
    end function
  end interface
end module
