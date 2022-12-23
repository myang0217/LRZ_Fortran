module mod_body
 implicit none
 type :: body
   character(len=4) :: units
   real :: mass
   real :: pos(3), vel(3)
 end type body
contains
 subroutine kick(bowling_ball, dp)
   type(body), intent(inout) :: bowling_ball
   real, intent(in) :: dp(3)
   integer :: i

   do i = 1, 3
     bowling_ball % vel(i) = bowling_ball % vel(i) + &
                             dp(i) / bowling_ball % mass 
   end do  
 end subroutine
end module
program derived_type
  use mod_body
  implicit none
  integer, parameter :: ndim = 120
  type(body) :: asteroids(ndim)
  type(body) :: my_ball, copy_of_ball
  real :: x(3)

  my_ball = body( 'MKSA', mass=1.8, pos=[ 0.0, 0.0, 0.5 ], & 
                   vel=[ 0.01, 4.0, 0.0 ] )
  x(1) = 0.0
  x(2) = -1.0
  x(3) = 0.0

  copy_of_ball = my_ball

  call kick(my_ball, x)

  write(*, fmt='(A,1X,F6.2,1X,3(F6.2),1X,3(F6.2))') copy_of_ball
  write(*, fmt='(A,1X,F6.2,1X,3(F6.2),1X,3(F6.2))') my_ball
end program

