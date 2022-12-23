module mod_triads
  use timer
  implicit none
  integer, parameter :: rk = kind(1.0), dk = kind(1.0d0), &
       ik = kind(1), lk = selected_int_kind(14)

contains
  
  subroutine array_init_rk(x)
    real(rk), intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init_rk
  subroutine array_init_dk(x)
    real(dk), intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init_dk
  subroutine array_init_cz(x)
    complex(dk), intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init_cz
  subroutine array_init_ik(x)
    integer(ik), intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init_ik
  subroutine array_init_lk(x)
    integer(lk), intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init_lk

  subroutine eval_triads_rk(a, b, c, d, nsize, dt, niter)
    real(rk), intent(in) :: a(*), b(*), c(*)
    real(rk), intent(inout) :: d(*)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter, nsize
    integer ::  it, i
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine eval_triads_rk
  subroutine eval_triads_dk(a, b, c, d, nsize, dt, niter)
    real(dk), intent(in) :: a(*), b(*), c(*)
    real(dk), intent(inout) :: d(*)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter, nsize
    integer ::  it, i
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine eval_triads_dk
  subroutine eval_triads_cz(a, b, c, d, nsize, dt, niter)
    complex(dk), intent(in) :: a(*), b(*), c(*)
    complex(dk), intent(inout) :: d(*)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter, nsize
    integer ::  it, i
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine eval_triads_cz
  subroutine eval_triads_ik(a, b, c, d, nsize, dt, niter)
    integer(ik), intent(in) :: a(*), b(*), c(*)
    integer(ik), intent(inout) :: d(*)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter, nsize
    integer ::  it, i
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine eval_triads_ik
  subroutine eval_triads_lk(a, b, c, d, nsize, dt, niter)
    integer(lk), intent(in) :: a(*), b(*), c(*)
    integer(lk), intent(inout) :: d(*)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter, nsize
    integer ::  it, i
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine eval_triads_lk
end module mod_triads
