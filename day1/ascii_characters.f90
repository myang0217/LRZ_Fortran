program ascii_characters
  implicit none
  integer, parameter :: amax=128
  integer :: i

  do i=0,amax-1
    write(*,fmt='(i3,2x,a)') i, achar(i)
  end do
end program
