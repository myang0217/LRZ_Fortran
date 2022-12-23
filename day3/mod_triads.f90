module mod_triads
  use timer
  implicit none
  double precision, pointer :: p(:) => null()
contains
  
  subroutine array_init(x)
    double precision, intent(inout) :: x(:)
    integer :: i
    do i=1, size(x)
       x(i) = 1
    end do
  end subroutine array_init
  subroutine assumed_shape_array(a, b, c, d, dt, niter)
    double precision, intent(in) :: a(:), b(:), c(:)
    double precision, intent(inout) :: d(:)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter
    integer :: nsize, it, i
    nsize = size(a)
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine assumed_shape_array
  subroutine contig_assumed_shape_array(a, b, c, d, dt, niter)
    double precision, intent(in), contiguous :: a(:), b(:), c(:)
    double precision, intent(inout), contiguous :: d(:)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter
    integer :: nsize, it, i
    nsize = size(a)
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine contig_assumed_shape_array
  subroutine pointer_intent_in(a, b, c, d, dt, niter)
    double precision, intent(in), pointer :: a(:), b(:), c(:), d(:)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter
    integer :: nsize, it, i
    nsize = size(a)
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine pointer_intent_in
  subroutine contig_pointer(a, b, c, d, dt, niter)
    double precision, intent(in), pointer, contiguous :: a(:), b(:), c(:), d(:)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter
    integer :: nsize, it, i
    nsize = size(a)
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
  end subroutine contig_pointer
  subroutine pointer_intent_inout(a, b, c, d, dt, niter)
    double precision, intent(inout), pointer :: a(:), b(:), c(:), d(:)
    double precision, intent(inout) :: dt
    integer, intent(in) :: niter
    integer :: nsize, it, i
    nsize = size(a)
    dt = dwalltime()
    do it=1, niter
       do i=1, nsize
          d(i) = c(i) + a(i)*b(i)
       end do
       call dummy(d, a)
    end do
    dt = dwalltime() - dt
    p => d
    d => c
    call dummy(d, a)
    d => p
    nullify(p)
  end subroutine pointer_intent_inout
end module mod_triads
