program solve_my_quadratic 
  implicit none
  integer :: io_stat
  real :: a , b , c
  real :: x1, x2
  intrinsic :: sqrt

do 
  write(*, fmt=*) 'Enter a, b, c: '
  !read and determine if a is exsiting
  read(*, fmt=*, iostat=io_stat) a, b, c

  if (io_stat /= 0) then
    write(*, fmt=*) 'No more input'
    exit
  end if 

  if ( b**2 - 4 * a * c < 0.0 ) then
    write(*, fmt=*) 'No real solutions'
  else 
    x1 = ( -b + sqrt(b**2 - 4. * a * c) ) / ( 2. * a )
    x2 = ( -b - sqrt(b**2 - 4. * a * c) ) / ( 2. * a )
    write(*, fmt=*) 'Solutions are: ', x1, x2
  end if
end do

end program