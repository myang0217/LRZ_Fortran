!
! demonstrate use of array intrinsic
! by computing certain statistics on scored tests
program scores
  implicit none
  integer, parameter :: n_stud = 3, n_test = 4
  integer :: is, it
  real :: all_scores(n_stud, n_test)
  logical :: above(n_stud, n_test)
  real :: average_per_test(n_test)
  real :: deviation_per_student(n_stud)
  real :: average_overall
  intrinsic :: abs, count, real, reshape, size, spread, sum
!
! test data
  all_scores = reshape( [ 85., 71., 66., 76., 45., 45., &
       90., 50., 21., 60., 80., 55. ], [ 3, 4 ])
!
! subproblem 1
  average_per_test = sum(all_scores, dim=1) / real(n_stud)
  write(*,*) 'Per-test average:'
  do it=1, n_test
     write(*,*) 'Test ',it,' has average ', average_per_test(it)
  end do
!
! subproblem 2
  average_overall = sum(all_scores) / real(size(all_scores))
  write(*,*) 'Overall average: ', average_overall
  
  above = (all_scores > average_overall)
  write(*,*) 'Number of scores above average: ',count(above)
  
!
! subproblem 3
  deviation_per_student = sum( abs(all_scores - &
       spread( sum(all_scores, dim=2) / real(n_test), dim=2, ncopies=4 ) &
       ), dim=2 ) / real(n_test)

  do is=1, n_stud
     write(*,*) 'Student ',is, ' has average deviation: ', &
          deviation_per_student(is)
  end do

!
! subproblem 4
  if (any(all(above, dim=2))) then
     write(*, *) 'At least one of the students was always above the overall average'
  else
     write(*, *) 'None of the students was always above the overall average'
  end if
end program scores
