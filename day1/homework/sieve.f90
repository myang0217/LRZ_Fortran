program sieve
  implicit none
  integer, parameter :: ip = selected_int_kind(8)
  integer(ip), parameter :: dim = 15000000 
  integer(ip) :: integer_array(dim)
  integer(ip) :: i, j

  integer_array(2) = 2
  integer_array(3) = 3

  prime: do i = 4, dim
            integer_array(i) = i
              do j = 2, floor(sqrt(real(i)))
                if ( integer_array(j) == 0 ) then
                  cycle
                else if ( mod(i, j) == 0 ) then
                  integer_array(i) = 0
                  cycle prime
                end if
              end do 
          end do prime

  !write(*,*) pack(integer_array, integer_array /= 0)
end program sieve