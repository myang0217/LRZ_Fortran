module mod_globaldata
  implicit none
  integer, parameter :: dm = 10000
  real, protected :: conversion_factor = 11.2
  real, private :: my_data(dm)
contains
  subroutine set(x,n)
    real, intent(in) :: x(*)
    integer, intent(in) :: n
    integer :: i
    do i=1, min(n, dm)
      my_data(i) = x(i)
    end do
  end subroutine set
  subroutine op1(x, n)
    real, intent(in) :: x(*)
    integer, intent(in) :: n
    integer :: i
    do i=1, min(n, dm)
      my_data(i) = x(i)*conversion_factor + my_data(i)
    end do
  end subroutine op1
  subroutine write_data(istart, iend)
    integer, intent(in) :: istart, iend
    integer :: i
    do i=istart,iend
      write(*, fmt='(i7,1x,f12.2)') i, my_data(i)
    end do
  end subroutine
  subroutine rescale(factor)
    real, intent(in) :: factor
    conversion_factor = conversion_factor * factor
  end subroutine
end module mod_globaldata
program global_data
  use mod_globaldata
  implicit none
  real :: x(dm)
  integer :: i
  do i=1,dm
    x(i) = real(i)
  end do
  call set(x, dm)
! the following not permitted due to the PRIVATE attribute
!  my_data(5) = 12.0 
  call op1(x, dm)
  call write_data(5,10)
  write(*,'(10(''-''))') 
! the following not permitted due to the PROTECTED attribute
!  conversion_factor = -11.2
  call rescale(-1.0)
  call op1(x, dm)
  call write_data(5,10)
end program


