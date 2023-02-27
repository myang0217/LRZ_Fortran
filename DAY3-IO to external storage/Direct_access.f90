program direct_access
    implicit none
    integer(kind=8) :: max_length
    real(kind=4) :: x(100), y(100)
    integer :: nr

    !initialize x by random numbers
    call random_number(x)
    call random_number(y)

    inquire(iolength=max_length) size(x), size(y), x, y

    open(unit=99, file='direct_data', status='replace', action='write', &
         access='direct', form='unformatted', recl=max_length)


    close(99)
    
end program direct_access