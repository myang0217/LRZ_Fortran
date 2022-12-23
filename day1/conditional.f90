program conditional
  implicit none
  integer :: index
  real :: x, y

  write(*, *) 'Please supply an integer value!'
  read(*, *) index

  if (index == 0) then
     y = 0.0
  else if (1 <= index .and. index <= 4) then
     y = 1.0
  else if (5 <= index) then
     y = 2.0
  else
     y = -1.0
  end if
  write(*, *) 'IF construct delivers the result ',y

  select case (index)
  case (0) 
     x = 0.0
  case (1:4)
     x = 1.0
  case (5:)
     x = 2.0
  case default
     x = -1.0
  end select
  write(*, *) 'SELECT CASE construct delivers the result ',x

end program conditional
