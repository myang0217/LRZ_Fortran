program arrays
  implicit none
  
  integer, parameter :: dm = 6
  real, dimension(dm) :: a
  integer :: i
  real :: t1
  
  a = [ 1.,3.,5.,7.,9.,11. ]

  write(*,*) 'start value of a:', a

  i = 2
  a(3) = 2.0
  t1 = a(3)
  a(i) = t1*3.0
  t1 = t1 + a(i+4)

  write(*,*) 'end value of a:', a
  write(*,*) 'value of t1:', t1
  
end program arrays
