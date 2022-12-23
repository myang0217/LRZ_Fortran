program sort_string
  implicit none
  integer, parameter :: s_len = 80, len_alph = 26
  character(len=s_len) :: s, s_sorted
  character(len=len_alph) uc
  character(len=len_alph) lc
  integer :: i, ic, inum(len_alph)
!
  uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  lc='abcdefghijklmnopqrstuvwxyz'
  do
     write(*, *) 'Enter an alphabetic string in quotes (if empty, program terminates):'
     read(*, *) s
!
!  Also try sentences with blanks inside. 
!  For termination, you need to explicitly enter '' 
!
     if (trim(s) == '') exit
     inum = 0                 ! all elements
     s_sorted(1:s_len) = ' '  
     do ic=1, len_alph
        do i=1, s_len
           if(s(i:i) == uc(ic:ic) .or. s(i:i) == lc(ic:ic)) then
              inum(ic) = inum(ic)+1
              s_sorted = trim(s_sorted) // lc(ic:ic)
           end if
        end do
     end do
     write(*, *) 'Sorted string: ',s_sorted
     write(*, *) 'Statistics:'
     do ic=1, len_alph
        if (inum(ic) > 0) &
             write(*, *) 'Letter ',lc(ic:ic),': ',inum(ic)
     end do
  end do
end program sort_string
