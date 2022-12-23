program execute_cmdline
  use, intrinsic :: iso_fortran_env
  implicit none
  integer :: stat, value, unit = 20
  logical :: dat_exists
!
! illustrates asynchronous execution of a program external 
! to Fortran
  call execute_command_line('/usr/bin/rm -f async_script.dat',wait=.true.,cmdstat=stat)
  call execute_command_line('./async_script.sh',wait=.false.,cmdstat=stat)
  write(*,*) 'Continuing execution in Fortran ...'
  value = 0
  if (stat == 0) then
     do 
        inquire (file='async_script.dat', exist=dat_exists)
        if (dat_exists) exit
     end do
     write(*,*) 'Wait a bit to assure file has actually been written.'
     call execute_command_line('/usr/bin/sleep 2',wait=.true.,cmdstat=stat)
     open (unit, file='async_script.dat', form='FORMATTED', status='OLD')
     read (unit, *) value
     close (unit, status='DELETE')
     write(*,*) 'File was processed with result: ', value 
  else
     stop 'could not invoke external program asynchronously'
  end if
end program execute_cmdline
