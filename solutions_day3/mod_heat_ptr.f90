module mod_heat_ptr
  implicit none
  private
  public :: heat_create, heat_destroy, heat_ival, heat_bval, heat_iter, &
       heat_print
  integer, parameter, public :: dk = selected_real_kind(12,100)
!
!> discretization of temperature field on a unit square
  real(dk), pointer, contiguous :: phi(:,:) => null(), phinew(:,:) => null(), &
       aux(:,:) => null()
!
!> space increments depend on discretization
  real(dk) :: dx, dy
!
!> iteration count, state checking, checkpointing settings, file names
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
!> create temperature data fields 
  subroutine heat_create(nx, ny)
    integer, intent(in) :: nx, ny
    if (nx < 3 .or. ny < 3) return
    if (associated(phi)) then
       if (sum(abs(shape(phi) - (/ nx, ny /))) > 0 ) deallocate(phi, phinew)
    end if
    if (.not. associated(phi)) &
         allocate(phi(nx, ny), phinew(nx, ny))
    dx = 1.0_dk / real(nx - 1, dk)
    dy = 1.0_dk / real(ny - 1, dk)
    num_iter = 0
  end subroutine heat_create
!
!> delete temperature data fields
  subroutine heat_destroy()
    if (associated(phi)) deallocate(phi, phinew)
  end subroutine heat_destroy
!
!> set initial values for interior of temperature field
!> if this is done, the iteration count is reset
  subroutine heat_ival(fival)
    procedure(f2) :: fival

    integer :: i, j
    real(dk) :: x, y

    if (.not. associated(phi)) return

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

    if (.not. associated(phi)) return
    select case (side)
    case ('n','N')
       do j = 1, size(phi,2)
          y =  dy * real(j - 1, dk)
          phi(1, j) = fbval(y)
          phinew(1, j) = fbval(y)
       end do
    case ('s','S')
       do j = 1, size(phi,2)
          y =  dy * real(j - 1, dk)
          phi(size(phi,1), j) = fbval(y)
          phinew(size(phi,1), j) = fbval(y)
       end do
    case ('w','W')
       do i = 1, size(phi,1)
          x =  dx * real(i - 1, dk)
          phi(i, 1) = fbval(x)
          phinew(i, 1) = fbval(x)
       end do
    case ('e','E')
       do i = 1, size(phi,1)
          x =  dx * real(i - 1, dk)
          phi(i, size(phi,2)) = fbval(x)
          phinew(i, size(phi,2)) = fbval(x)
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
!> absolute maximum deviation from the last iteration 
  real(dk) function heat_iter(dt, num)
    real(dk), intent(in) :: dt
    integer, intent(in) :: num

    integer :: it, nx, ny
    logical :: ex
    real(dk) :: dphi, dphimax, d2x, d2y
    
!
!   proceed with calculation
    d2x = 1.0_dk/(dx*dx); d2y = 1.0_dk/(dy*dy)
    do it=1, num
       num_iter = num_iter + 1
     
       call stencil(phi, phinew, d2x, d2y, dt, dphimax)

       aux => phi
       phi => phinew
       phinew => aux
    end do
    heat_iter = dphimax
  contains
    subroutine stencil(phi, phinew, d2x, d2y, dt, dphimax)
      integer :: i, j
      real(dk), intent(in) :: d2x, d2y, dt
      real(dk), intent(out) :: dphimax
      ! because the following are no pointers, and are contiguous,
      ! optimization can now proceed full force ahead ...
      real(dk), contiguous, intent(in) :: phi(:,:)
      real(dk), contiguous, intent(inout) :: phinew(:,:)
!      real(dk), intent(in) :: phi(:,:)
!      real(dk), intent(inout) :: phinew(:,:)
   
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
     
    end subroutine
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
end module mod_heat_ptr
