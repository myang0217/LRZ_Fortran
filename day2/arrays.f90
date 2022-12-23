program arrays
  implicit none
  integer, parameter :: nb = 2, ld = 1, nd=6, nv=10
  real :: a(nd), bb(nb, -ld:ld)
  real  :: v(nv)
  integer :: iv(4)
  integer :: i, j
  intrinsic :: lbound, ubound, shape, size


  write(*, *) 'Lower bounds of bb: ', lbound(bb)
  write(*, *) 'Upper bounds of bb: ', ubound(bb)
  write(*, *) 'Lower bounds of (bb): ', lbound( (bb) )
  write(*, *) 'Upper bounds of (bb): ', ubound( (bb) )
  write(*, *) 'Shape of b: ', shape(bb)
  write(*, *) 'Size of b: ', size(bb)


  a =  [ 1.,3.,5.,7.,9.,11. ]
  bb = reshape([ 1,3,5,7,9,11 ], shape=[ 2,3 ])

  do i = 1, nd
    write(*,*) 'i, a(i): ',i,a(i)
  end do
  do j = -ld, ld
    do i = 1, nb
      write(*,*) 'i, j, b(i,j): ',i,j,bb(i,j)
    end do
  end do

  v = [ (real(i), i=1, 10) ] 
  iv = [ 2, 3, 9, 5 ]
  write(*,*) 'v(iv) = ',v(iv)
  iv = [ 2, 3, 9, 2 ]
  write(*,*) 'v(iv) = ',v(iv)
  

end program
