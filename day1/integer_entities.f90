program integer_entities
  implicit none
!
! declarations
  intrinsic :: int
  integer, parameter :: ip = 42  ! named constant
  integer :: i                   ! could also write 'integer i' here

  integer, parameter :: d=12, lk=selected_int_kind(d)
!  integer, parameter :: lk=8
! avoid defining lk via the above commented line
! some processors may use a KIND value 8 for 64 bit integers, others 
! a different value

  integer(kind=lk) :: mv
! allow mv to contain at least integers between -10**12+1 and 10**12-1

!
! executable statements follow
  write(*,*) 'expression -121+45 of literals: ', -121 + 45

  i = -121 + 45                  ! assignment
  i = i + 4     
  write(*,*) 'should be -121 + 45 - 4 = -72: ', i

  i = i + ip
! ip = i + 4
! the above commented line is non-conforming and would not compile


! use parameterized integer
  mv = 12345678900_lk
!  mv = 12345678900
! the above line will not compile if a default kind integer cannot represent
! the number
  write(*,*) 'mv has value ', mv

!  mv = 4_lk
!
! Guarding against overflow
  if (abs(mv) < huge(i)) then
    i = mv
    write(*,*) 'value of mv copied to i has value ', i
  else
    write(*,*) 'no copy of mv due to integer overflow'
  end if

! BOZ (Binary/Octal/Hexadecimal) constants:
! require use of intrinsic INT
  write(*,*) 'decimal value for binary 100110 is 38: ', int(b'100110')
!  write(*,*) 'decimal value for binary 100110 is 38: ', b'100110'
! the previous commented line is non-conforming, but often supported 
! as an extension
  write(*,*) 'decimal value for binary 3407 is 1799: ', int(o'3407')
  write(*,*) 'decimal value for hex fb92   is 64402: ', int(z"fb92")


end program integer_entities
