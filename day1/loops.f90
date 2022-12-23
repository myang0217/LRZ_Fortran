program loops
  implicit none
  integer, parameter :: ndim = 5
  integer :: i, j, icount, ifound, k, n
  real :: t, x
  real :: a(ndim)
  
!
! nested loops
  n = ndim
  icount = 0

outer: do j=1,n
    if (j>1) write(*, *) 'k: ',k
    t = 0.2
    do k=1,n
      t = t - cos(real(k)*real(j))
      icount = icount + 1
      if (t < 0.0) cycle outer    
    end do
    a(j) = a(j) + sqrt(t) * real(j)
  end do outer 
  write(*, *) 'k: ',k
  write(*, *) 'executed ',icount, ' out of ',n*n,' iterations.'
  write(*, *) 'a: ',a

!
! Loops nested inside a block construct

  ifound = 0
  n = ndim
  do i = 1, n
     a(i) = real(i)
  end do 
  x = real(3)
!  x = real(7)
finder : block
! Declarations of variables are permitted inside the construct.
! They exist while the construct executes.
  integer :: i
  do i=1,n
    if (x == a(i)) then
        ifound = i
        exit finder    
    end if
  end do
  write(*,*) 'Not found'
end block finder 
  if (ifound > 0) write(*,*) 'Found: ', ifound

end program
