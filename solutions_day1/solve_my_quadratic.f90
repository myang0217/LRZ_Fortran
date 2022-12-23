program solve_my_quadratic 
  implicit none
  real :: a, b, c
  real :: disc, x1, x2
  integer :: ios
  intrinsic :: sqrt


  processing: do 
    write(*, *) 'Enter coefficients a, b, c:'
    read(*, fmt=*, iostat=ios) a, b, c
    if (ios /= 0) exit processing  ! a, b, c are then undefined

    disc = b**2 - 4. * a * c

    if (disc >= 0.0) then
      x1 = ( -b + sqrt(disc) ) / ( 2. * a )
      x2 = ( -b - sqrt(disc) ) / ( 2. * a )
      write(*,'(A,E17.10,2X,E17.10)') 'Solutions are: ', x1, x2
    else
      write(*,*) 'No real-valued solution exists'
    end if
  end do processing
  write(*,*) 'Exiting processing loop'
end program

