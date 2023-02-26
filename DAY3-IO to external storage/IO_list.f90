program IO_list
    implicit none
    integer :: a, b, c
    character(len=40) :: io_three_int
    integer, allocatable :: cvs_list(:)

    io_three_int = '("a = ", i3, " b = ", i3, " c = ", i3)'
    a = 1
    b = 2
    c = 3
    !write(*, fmt=* ) a, b, c
    write(*, fmt=io_three_int ) a, b, c

    allocate(cvs_list(5))
    cvs_list = [1, 2, 3, 4, 5]
    write(*, '(*(I2,:,'','' ))') cvs_list
end program IO_list