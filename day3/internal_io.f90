program internal_io
  character(len=range(1)+1) :: i_char
  character(len=100) :: my_fmt
  integer :: i, iw
  integer, allocatable :: iarr(:)
  
! integer to character conversion
  i = -1334
  do while (abs(i) < huge(1)/10)
     write(i_char, fmt='(i0)') i
     write(*, fmt='(a)') i_char
     i = 10*i
  end do

! dynamic format generation
  allocate(iarr(7))
  iarr = (/ (-17*i, i=1, 7) /)
  iw = int(log(maxval(real(abs(iarr))))/log(10.)+0.1)+2
  write(*, *) 'width: ',iw
  write(my_fmt, fmt='(''('',i0,''i'',i0,'')'')') size(iarr), iw
  write(*, fmt=my_fmt) iarr
end program
