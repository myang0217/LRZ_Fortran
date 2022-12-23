program real_entities
  implicit none
!
! declarations 
  integer, parameter :: dk = kind(1.0d0)
! the following is an alternative which on most systems
! should give the same value as the line above
! integer, parameter :: dk = selected_real_kind(10,99)

  real :: x
  real(kind=kind(1.0)) :: y
  complex(kind=kind(1.0)) :: z

  double precision :: dx
  real(kind=dk) :: dy
  integer :: i


!
! executable statements follow
  write(*,*) 'Printing some constants:'
  write(*,*) 4e+02
  write(*,*) -3.1415926
  write(*,*) .56e-12
  write(*,*) 6.


  x = 4.5e2
  y = x * 5.4 + 1.0e2
  write(*,*) 'Printing values of default real variables:'
  write(*,*) x, y

  dx = 4.5e2_dk
  dy = dx * 5.4_dk + 1.0d2
  write(*,*) 'Printing values of KIND ',dk,' real variables:'
  write(*,*) dx, dy

  z = (1.0_dk, 2.0_dk)
  write(*,*) 'Printing value of KIND ',kind(z),' complex variable:'
  write(*,*) z

!
! Beware loss of precision in the following lines
! even though variables are double precision
  dx = 4.5e2
  dy = dx * 5.4 + 1.0d2
  write(*,*) 'Loss of precision for not exactly representible values: ', dx, dy
!
! another example for loss of precision
  y = 1.1
  dy = y
  write(*,*) 'Difference:', abs(dy - 1.1_dk)

!
! conversion to integer
  y = -1.1
  i = y
  write(*,*) y, ' implicitly converted to integer: ', i

end program real_entities
