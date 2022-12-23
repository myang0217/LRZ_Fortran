program test_rational
  use rational
  implicit none
  integer, parameter :: lk = selected_int_kind(10)
  type(fraction) :: f, g, h, x
  real :: y
  integer(lk) :: ifrac(2)

  f = fraction(8,2); call write(f)  ! Should be 4/1
  f = fraction(7,2); call write(f)  ! Should be 7/2
  f = fraction(-8,2); call write(f) ! Should be -4/1
  f = fraction(8,-2); call write(f) ! Should be -4/1
  f = fraction(8,0); call write(f)  ! Should be 1/0
  f = fraction(-2,8); call write(f) ! Should be -1/4
  f = fraction(2,-8); call write(f) ! Should be -1/4
  f = fraction(4,1); call write(f)  ! Should be 4/1
  
  f = fraction(8_lk, 5_lk); g = fraction(3_lk, 5_lk)
  h = f + g; call write(h)  ! Should be 11/5
  f = fraction(8_lk, 5_lk); g = fraction(2_lk, 5_lk)
  h = f + g; call write(h)  ! Should be 2/1
  f = fraction(8_lk, 5_lk); g = fraction(2_lk, 5_lk)
  h = f - g; call write(h)  ! Should be 6/5
  f = fraction(8_lk, 5_lk); g = fraction(3_lk, 5_lk)
  h = f - g; call write(h)  ! Should be 1/1
  f = fraction(8_lk, 5_lk); g = fraction(3_lk, 5_lk)
  h = f * g; call write(h)  ! Should be 24/25
  f = fraction(8_lk, 5_lk); g = fraction(3_lk, 5_lk); h = fraction(5_lk, 12_lk)
  x = f * h + g; call write(x)    ! Should be 19/15
  x = f * (h + g); call write(x)  ! Should be 122/75
  x = g + f * h; call write(x)    ! Should be 19/15
  f = fraction(8_lk, 5_lk); g = fraction(3_lk, 5_lk)
  h = f / g; call write(h)  ! Should be 8/3
  y = h; write(*,*) y       ! Should print 2.6666667 
                            ! (or indistinguishable real value)
  ifrac = h; write(*,*) ifrac   ! Should print 8   3
  h = .inv. h; call write(h) ! Should be 3/8
  h = 4
  y = h ; write(*,*) y       ! Should print 4.00000000
 end program test_rational
