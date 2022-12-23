module mod_rand
  implicit none
contains
  subroutine random_int(irand, lower, upper)
    intrinsic :: random_number
    integer, intent(out) :: irand(:) ! an assumed shape array
                                     ! with implicit shape information
    integer, intent(in) :: lower, upper
!
    integer :: i
    real :: rand(size(irand))
!
    call random_number(rand)
    if (lower <= upper) then
       irand = floor((upper-lower+1)*rand + lower)
    else
       irand = floor((lower-upper+1)*rand + upper)
! Question: why not int?
    end if
  end subroutine random_int
end module mod_rand
