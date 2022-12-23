module mod_transpose_rank3
  implicit none
contains
  subroutine transpose_rank3(a, b)
    real, allocatable, intent(out) :: a(:,:,:)
    real, intent(in) :: b(:,:,:)

    allocate(a(size(b,3),size(b,2),size(b,1)))
    a = reshape(b,shape(a),order=[3,2,1])

  end subroutine transpose_rank3
end module
program test_transpose
  use mod_transpose_rank3
  implicit none
  real :: b(3,4,5)
  real, allocatable :: a(:,:,:)
  real :: diff
  integer :: i1, i2, i3
  do i3=1,5
    do i2=1,4
      do i1=1,3
        b(i1, i2, i3) = 1.*i1 + 10.*i2 + 100.*i3
      end do
    end do
  end do

  call transpose_rank3(a, b)
  write(*,*) 'Shape: ',shape(a)
  diff = 0.
  do i3=1,5
    do i2=1,4
      do i1=1,3
        diff = max(diff, abs(a(i3,i2,i1)-b(i1,i2,i3)))
      end do
    end do
  end do
  write(*,*) 'Diff: ',diff
end program
