module mod_heat_static
  implicit none
  private
  public :: heat_ival, heat_bval, heat_iter, heat_print
  integer, parameter, public :: dk = selected_real_kind(12,100)
  integer, parameter, public :: ndim = 10
!
! FIXME: add declarations for fields etc.
!
! FIXME: add abstract interfaces for procedure arguments
  abstract interface
  end interface
contains
!
!> set initial values for interior of temperature field
  subroutine heat_ival(fival)
    procedure(f2) :: fival

    integer :: i, j
    real(dk) :: x, y

! FIXME: add code that initializes phi(i,j) in the interior

    num_iter = 0
  end subroutine heat_ival
!
!> set boundary values of temperature field
!> these must be preserved throughout any field updates
  subroutine heat_bval(side, fbval)
    character, intent(in) :: side
    procedure(f1) :: fbval

    integer :: i, j
    real(dk) :: x, y

    select case (side)
! FIXME: add code that calculates boundary values
    end select
  end subroutine heat_bval
!
!> perform a specified number of Jacobi iterations 
!> with a provided time increment and return the 
!> absolute maximum deviation from the last iteration.
!> Note that the number of arguments differs from that on 
!> the slides, to enable inlining of a configurable
!> number of iterations.
  real(dk) function heat_iter(dt, num)
    real(dk), intent(in) :: dt
    integer, intent(in) :: num

    real(dk) :: d2x, d2y
    
    d2x = 1.0_dk/(dx*dx); d2y = 1.0_dk/(dy*dy)
    do it=1, num
! FIXME: write code that implements the stencil
!        some additional declarations may also be needed above
    end do
    heat_iter = dphimax
  end function heat_iter
!
!> write out (a subset of) values of field phi
  subroutine heat_print()
    integer, parameter :: pmax = 10, qmax = 30
    integer :: i, imax, jmax
    jmax = min(pmax, size(phi,2))
    imax = min(qmax, size(phi,1))
    write(*,fmt='(''Temperature field after iteration '',i0,'':'')') num_iter
    do i=1, imax
       write(*,fmt='(10(f7.4,1x))') phi(i,1:jmax)
    end do
    write(*,fmt='(80(''-''))')
  end subroutine heat_print
end module mod_heat_static
