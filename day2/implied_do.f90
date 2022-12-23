program implied_do
  implicit none

  integer :: i, j
  real :: a(10), b(5, 10)
  real :: aa(10) = [ ( real(i), i=1, size(aa) ) ]
  real :: bb(5, 10) 
!  real :: bb(5, 10) = reshape( [ ( ( sin(real(i)), i=1,size(b, 1) ), j=1,size(b, 2) ) ], &
!                              shape(b) ) 

  data  ( a(i), i=2,4 )                   / 1.0, 2.0, 3.0 / 
  data  ( ( b(i, j), i=1,5 ), j=1,10,2 )  / 20*0.0, 5*1.0 /



  bb = reshape( [ ( ( sin(real(i)), i=1,size(b, 1) ), j=1,size(b, 2) ) ], &
               shape(bb) )

  write(*, '(10F6.2,/)') a
  write(*, '(10F6.2,/)') aa
  do j=1,size(b, 2)
!   NOTE: every second row of b is undefined
    write(*, '(5F9.5,2X,5F9.5)') b(:,j), bb(:,j)
  end do
end program implied_do
