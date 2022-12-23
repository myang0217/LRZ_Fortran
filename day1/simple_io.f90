program simple_io
  implicit none
  real :: x1, x2
  integer :: k

  ! the following three statements do the same thing
  write(*,fmt=*) 'Hello'
  write(*,*)     'Hello'
  print *,       'Hello'

  x1 = 1.5; x2 = -0.5
  ! write formatted floating point data and string constant
  write(*,fmt='(A,F12.5,1X,E12.5)') 'Solutions are ', x1, x2
  
  ! the following causes problems due to an insufficiently specified descriptor
  write(*,fmt='(A,F7.6)') 'See stars only: ', x1
  
  ! write integer data
  k = -42
  write(*,fmt='(A,I5)') 'Negative answer:', k

end program simple_io
