module mod_proc
  implicit none
  integer, parameter :: dm = 3, da = 12
contains
  subroutine proc(a, n)
    real a(*)
    integer :: n
    real wk1( int(log(real(n))/log(10.)) )
    real wk2( sfun(n) )
    write(*,*) 'Size of wk1 is ', size(wk1, 1)
    write(*,*) 'Size of wk2 is ', size(wk2, 1)
  end subroutine proc
!
! the PURE attribute in the next statement is obligatory 
  pure integer function sfun(n)
    integer, intent(in) :: n
    sfun = dm * n + da
  end function sfun
end module mod_proc
program p
  use mod_proc
  implicit none
  real a(101)
  call proc(a, size(a,1))
end program
