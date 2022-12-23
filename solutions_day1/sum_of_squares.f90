program sum_of_squares
  implicit none
  integer, parameter :: ik = selected_int_kind(7)
  integer, parameter :: lk = selected_int_kind(14)
  integer, parameter :: wp = ik
!  integer, parameter :: wp = lk
  integer(wp), parameter :: dm = 10_wp, limit = 100_wp
!  integer(wp), parameter :: dm = 100000_wp, limit = 10000000000_wp
  integer(wp) :: j, jsum
  integer(wp) :: sq(dm)

  jsum = 0
  sq = 0
  do j = 1, dm
    sq(j) = j*j
    jsum = jsum + sq(j)
    if (jsum > limit) exit
    write(*, '("Sum(j=",i5,") = ",i10)') j,jsum
!    write(*, '("Sum(j=",i0,") = ",i0)') j,jsum
!  i0 format writes as many characters as necessary to represent the value
  end do
  
end program
