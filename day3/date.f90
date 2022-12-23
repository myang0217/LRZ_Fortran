module mod_date
  implicit none
  integer, parameter :: lk = selected_int_kind(12), nmon = 12
  character(len=3), parameter :: all_months(nmon) = &
       (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
  integer, parameter :: days_per_month(nmon) = &
       (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  type :: date
     private
     integer :: day = 0

     character(len=3) :: month
!    avoids ambiguities resulting from differing day/month 
!    ordering conventions 

     integer(lk) :: year
!    allow astrophysical simulations to run without overflow
  end type date
contains
  type(date) function set_date_to(day, mon, year)
    integer, intent(in) :: day
    character(len=3), intent(in) :: mon
    integer(kind=lk), intent(in) :: year

    logical :: leap_year
    integer :: i, maxd

    leap_year = .false.
    if (mod(year,4_lk) == 0 .and. mod(year,400_lk) /= 0) then
       leap_year = .true.
    end if
    
    do i = 1, nmon
       if (mon == all_months(i)) exit
    end do
    
    if (i > nmon) return
    maxd = days_per_month(i) 
    if (leap_year .and. i == 2) maxd = 29
    if (day <= 0 .or. day > maxd) return
   
    set_date_to = date(day, mon, year)
  end function set_date_to
  logical function date_is_defined(this)
    type(date), intent(in) :: this
    date_is_defined = .true.
    if (this%day == 0) date_is_defined = .false.
  end function date_is_defined
  subroutine print_date(this)
    type(date) :: this
    if (date_is_defined(this)) then
       write(*,*) this%month, this%day, this%year
    else
       write(*,*) 'Date is undefined'
    end if
  end subroutine print_date
end module mod_date

program test_date
  use mod_date
  implicit none
  type(date) :: mydate

  mydate = set_date_to(12, 'Jan', 2012_lk)
  call print_date(mydate)
  mydate = set_date_to(31, 'Aug', 2012_lk)
  call print_date(mydate)
  mydate = set_date_to(31, 'Sep', 2012_lk)
  call print_date(mydate)
  mydate = set_date_to(29, 'Feb', 2012_lk)
  call print_date(mydate)
  mydate = set_date_to(29, 'Feb', 2013_lk)
  call print_date(mydate)
  mydate = set_date_to(29, 'Feb', 2000_lk)
  call print_date(mydate)
  mydate = set_date_to(13, 'Fxx', 2012_lk)
  call print_date(mydate)

!
! the following two statements will not compile 
! since components of date are private
!  mydate = date(12,'Jan',2012_lk)
!  write(*,*) mydate


end program test_date
