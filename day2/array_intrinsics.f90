program array_intrinsics
  implicit none
  
  integer :: a(3,2) 
  logical :: amask(3,2)
  integer :: vector(6) 
  integer :: result(6)

  integer :: v(2, 4)
  logical :: vmask(2, 4) 

!
! assign input values
  a = reshape( (/ 1, 2, 3, 4, 5, 6 /), (/ 3, 2 /))
  amask  =  reshape( (/ .true., .true., .true., &
       .false., .false., .true.  /), (/ 3, 2 /))
  vector = (/ 0, 0, 0, 0, 7, 8 /)
  vmask = reshape( (/ .true., .true., .false., &
       .true., .true., .true., .true., .false. /), (/ 2, 4 /) ) 

!
! pack
  write(*,*) 'Size of packed vector: ',size(pack(a, amask))
  write(*,*) 'Value of packed vector: ',pack(a, amask)
  if (size(pack(a, amask, vector)) == 6) then
     result = pack(a, amask, vector)
  else
     stop 'ERROR'
  end if
  write(*,*) 'Value of packed vector with padding: ',result
!
! unpack
  v(1, 2) = 9; v(2, 4) = 10
  v = unpack(result, vmask, v)
  write(*,*) 'Value of unpacked vector/masked v: ', v
end program
