module mod_body_bm
  implicit none

  type :: body_s
     character(len=4) :: units
     real :: mass
     real :: pos(3), vel(3)
  end type body_s

  type :: body_p (k, ntraj)
     integer, kind :: k = kind(1.0)
     integer, len :: ntraj = 1
     character(len=4) :: units
     real(kind=k) :: mass(ntraj)
     real(kind=k) :: pos(ntraj,3), vel(ntraj,3)
  end type body_p
contains
  subroutine kick_s(bowling_ball, dp)
    type(body_s), intent(inout) :: bowling_ball(:)
    real, intent(in) :: dp(:,:)
    integer :: i, j

    if (size(dp,2) >= size(bowling_ball) .and. size(dp,1) >= 3) then
       do j = 1, size(bowling_ball)
          do i = 1, 3
             bowling_ball(j) % vel(i) = bowling_ball(j) % vel(i) + &
                  dp(i, j) / bowling_ball(j) % mass
          end do
       end do
    end if
  end subroutine kick_s
  subroutine kick_p(bowling_ball, dp)
    type(body_p(k=kind(1.0), ntraj=*)), intent(inout) :: bowling_ball
    ! kind parameter is a compile time constant. Each value needs a separate procedure;
    ! length parameter is assumed from actual argument at run time.
    real, intent(in) :: dp(:,:)
    
    integer :: i, j

    if (size(dp,1) >= bowling_ball%ntraj .and. size(dp,2) >= 3) then
!       write(*,*) 'shape of subarray is', shape(bowling_ball % vel), 'iterations: ',bowling_ball%ntraj
       do i = 1, 3
          do j = 1, bowling_ball%ntraj
             bowling_ball % vel(j, i) = bowling_ball % vel(j, i) + &
                  dp(j, i) / bowling_ball % mass(j)
          end do
       end do
!       write(*,*) 'completed loop with ',bowling_ball%ntraj,' iterations.'
   else
       stop 'dp argument is not of sufficient shape.'
    end if
  end subroutine kick_p
end module mod_body_bm
