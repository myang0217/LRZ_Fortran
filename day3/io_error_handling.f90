program io_error_handling
  implicit none
!
! illustrates some styles of I/O error handling
  integer, parameter :: strmx=128
  integer :: ios, iu, irec
  character(len=strmx) :: errstr
  real :: x

!
! attempting to open a nonexistent file
  iu = 20
  open(iu, file='input.dat', action='READ', form='FORMATTED', &
           status='OLD', iostat=ios, iomsg=errstr)
  if (ios /= 0) then
    write(*,*) 'OPEN failed with error/message: ', ios, trim(errstr)
    error stop 1
  end if

  irec = 0
ioloop : do
  read(iu, fmt=*, iostat=ios, iomsg=errstr) x
  if (ios /= 0) then
    if (is_iostat_end(ios)) exit ioloop 
    write(*,*) 'READ failed with error/message: ', ios, trim(errstr)
    error stop 1
  end if
  irec = irec + 1
  write(*, *) 'record no. ',irec, ' has value ', x
end do ioloop

  close(iu)

end program io_error_handling
