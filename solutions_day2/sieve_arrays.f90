program Sieve_Of_Eratosthenes
  use ftn_getopt
  implicit none
  
!  Find prime numbers using array processing
!  with array intrinsics.

!  Strike the Twos and strike the Threes
!  The Sieve of Eratosthenes!
!  When the multiplies sublime
!  The numbers that are left, are prime.

!            From:  Drunkard's Walk, by Frederik Pohl

  integer, allocatable :: numbers(:)
  type(opt_t) :: options(2)

  integer         :: i, number_of_primes, ac
  integer         :: istat, ilen, ival
  integer         :: variant = 1, last_number = 0

  options = optinit( [ 'lastnum', 'variant' ], [ 'integer', 'integer' ] )
  call optarg(options)
  last_number = 0
  call optval(options(1), last_number)
  if (last_number <= 0) stop 'sieve_arrays: last number must be positive'
  call optval(options(2), variant)
  write(*,*) 'Executing variant ', variant

  call execute_sieve(last_number)

contains
  subroutine execute_sieve(last_number)
    !   automatic array with sufficient space
    integer :: last_number
    integer :: numbers(last_number)


    !  Initialize numbers array to 0, 2, 3, ..., last_number--
    !  Zero instead of 1 because 1 is a special case.
    numbers = (/ 0, (ac, ac = 2, last_number) /)
    select case (variant)
       !
       ! first variant:
    case (1)
       do i = 2, last_number
          if (numbers(i) /= 0) then               ! if this number is prime
             numbers(2*i : last_number : i) = 0   ! eliminate all multiples
          endif
       end do
       !
       ! second variant:
    case (2)
       do i = 2, last_number
          where (numbers*i <= last_number) 
             numbers(numbers*i) = 0
          end where
       enddo
       !
       ! third variant
    case (3)
       forall(i=2:last_number,numbers(i)/=0) numbers(i*2:last_number:i) = 0
       !
       !
    case default
       stop 'sieve_arrays: variant must be 1, 2, or 3.'
    end select
    !
    !  Count the primes.
    number_of_primes = count (numbers /= 0)
    !
    !  Gather them into the front of the array.
    numbers(1:number_of_primes) = pack(numbers, numbers /= 0)
    !
    !  Print them
    write(*,*) 'There are ', number_of_primes, &
         ' prime numbers less than or equal to ', last_number
    write(*,fmt='(5i10)') numbers(1:number_of_primes)

    !  Sample output:
    ! There are           25  prime numbers less than         100
    !         2         3         5         7        11
    !        13        17        19        23        29
    !        31        37        41        43        47
    !        53        59        61        67        71
    !        73        79        83        89        97
  end subroutine execute_sieve
  subroutine abort()
    write(*,'(''Usage: sieve_arrays -len=<positive integer>'')') 
    stop 1
  end subroutine abort
end program Sieve_Of_Eratosthenes
