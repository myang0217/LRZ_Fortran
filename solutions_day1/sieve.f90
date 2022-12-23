program sieve
  implicit none
  integer, parameter :: last_number = 100
!  integer, parameter :: last_number = 12534
  integer :: primes(last_number)
  integer :: num_primes
  
  integer :: i, k

  primes(1) = 0
  do i = 2, last_number
     primes(i) = i
  end do
    !
    ! for a non-prime index, the array value will be 
    ! set to zero. This means that multiples of 
    ! each non-zero entry starting are zeroed, 
    ! starting from the beginning of the array
  do i = 1, last_number
     if (primes(i) /= 0) then
        k = 2
        do 
           if (k*i > last_number) exit
           primes(k*i) = 0
           k = k + 1
        end do
     end if
  end do
  !
  ! now compress into contiguous subarray by eliminating 
  ! zero entries
  primes(1) = 2
  k = 1
  do i = 3, last_number
     if (primes(i) /= 0) then
        k = k + 1
        primes(k) = primes(i)
     end if
  end do
  num_primes = k
  write(*, *) 'number of primes <= ',last_number,':',num_primes
  write(*, *) primes(1:num_primes)
end program sieve
