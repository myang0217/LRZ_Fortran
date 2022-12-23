module mod_heat_static
  implicit none
  private
  public :: heat_ival, heat_bval, heat_iter, heat_print
  integer, parameter, public :: dk = selected_real_kind(12,100)
  integer, parameter, public :: ndim = 150
!
!> discretization of temperature field on a unit square
  real(dk) :: phi(ndim,ndim), phinew(ndim,ndim)
!
!> space increments depend on discretization
  real(dk) :: dx = 1.0_dk / real(ndim - 1, dk), &
              dy = 1.0_dk / real(ndim - 1, dk)
!
!> iteration count 
  integer, public, protected, save :: num_iter = 0
!
!> abstract interfaces for procedure arguments
  abstract interface
     pure function f1(x) result(f)
       import :: dk
!  dk is not available via host association, hence must be imported
       real(dk), intent(in) :: x
       real(dk) :: f
     end function f1
     pure function f2(x, y) result(f)
       import :: dk
       real(dk), intent(in) :: x, y
       real(dk) :: f
     end function f2
  end interface
contains
!
!> set initial values for interior of temperature field
!> if this is done, the iteration count is reset
  subroutine heat_ival(fival)
    procedure(f2) :: fival

    integer :: i, j
    real(dk) :: x, y

    do j = 2, size(phi,2) - 1
       y = dy * real(j - 1,dk)
       do i = 2, size(phi,1) - 1
          x = dx * real(i - 1,dk)
          phi(i,j) = fival(x,y)
       end do
    end do

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
    case ('n','N')
       do j = 1, size(phi,2)
          y =  dy * real(j - 1, dk)
          phi(1, j) = fbval(y)
       end do
    case ('s','S')
       do j = 1, size(phi,2)
          y =  dy * real(j - 1, dk)
          phi(size(phi,1), j) = fbval(y)
       end do
    case ('w','W')
       do i = 1, size(phi,1)
          x =  dx * real(i - 1, dk)
          phi(i, 1) = fbval(x)
       end do
    case ('e','E')
       do i = 1, size(phi,1)
          x =  dx * real(i - 1, dk)
          phi(i, size(phi,2)) = fbval(x)
       end do
    case default
       write(*,*) "mod_heat::heat_bval: incorrect value for argument SIDE."
       write(*,*) "                     Aborting execution."
       stop
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

    integer :: i, j, it
    real(dk) :: dphi, dphimax, d2x, d2y
    
    d2x = 1.0_dk/(dx*dx); d2y = 1.0_dk/(dy*dy)
    do it=1, num
       num_iter = num_iter + 1
       dphimax = 0.0_dk
       do j = 2, size(phi,2) - 1
          do i = 2, size(phi,1) - 1
             dphi = dt * ( &
                  (phi(i+1, j) + phi(i-1, j) - 2.0_dk*phi(i,j))*d2x + &
                  (phi(i, j+1) + phi(i, j-1) - 2.0_dk*phi(i,j))*d2y )
             dphimax = max(dphimax, abs(dphi))
             phinew(i, j) = phi(i, j) + dphi
          end do
       end do
       phi(2:size(phi,1)-1,2:size(phi,2)-1) = &
            phinew(2:size(phi,1)-1,2:size(phi,2)-1)
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
