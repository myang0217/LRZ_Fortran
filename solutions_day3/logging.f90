module logging
  implicit none
  private
  public :: logging_unit, logging_entry, logging_finish
  integer, parameter :: max_appname_len=65, max_logname_len = max_appname_len + 19
  integer :: v(8), ierr
  character(len=3) :: month(12) = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', &
                                    'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
contains
  integer function logging_unit(application)
    character(len=*), intent(in) :: application
!
    character(len=max_logname_len) :: logfile_name
    character(len=1024) :: err
    logical :: ex, op
    call date_and_time(values=v)
    write(logfile_name, &
         fmt='(''log_'',a,''_'',i2.2,''_'',a3,''_'',i4.4,''.txt'')') &
         trim(adjustl(application)),v(3),month(v(2)),v(1)
    open(newunit=logging_unit, iostat=ierr, iomsg=err, file=logfile_name, &
         action='WRITE', form='FORMATTED', status='NEW')

    if (ierr /= 0) then
       write(*,*) 'logging::logging_unit - I/O error: ',trim(err)
       stop 'Terminating program.'
    end if
  end function logging_unit
  subroutine logging_entry(iunit, message)
    integer, intent(in) :: iunit
    character(len=*), intent(in) :: message
!   should probably check whether unit is connected etc.
    call date_and_time(values=v)
    write(unit=iunit,iostat=ierr, fmt='(3(I2.2,'':''),1X,A)') v(5:7),message
  end subroutine logging_entry
  subroutine logging_finish(iunit)
    integer, intent(in) :: iunit
    character(len=1024) :: fname
    inquire(iunit, name=fname)
    write(*,*) 'logging::logging_finish: Closing ',trim(fname)
    close(unit=iunit, iostat=ierr)
  end subroutine logging_finish
end module logging
program test_logging
  use logging
  implicit none
  integer :: iu(2), k
  iu(1) = logging_unit('app_t1') ; iu(2) = logging_unit('Pheww')
  
  call logging_entry(iu(1), 'This is not an error')
  call logging_entry(iu(1), 'This is a critical error')
  call logging_entry(iu(2), 'Please inform your hardware vendor')
  
  call logging_finish(iu(1)); call logging_finish(iu(2))
     
end program test_logging
