program IO_list
    implicit none
    integer :: a, b, c
    character(len=40) :: io_three_int
    integer, allocatable :: cvs_list(:)
    integer :: i(24)

    io_three_int = '("a = ", i3, " b = ", i3, " c = ", i3)'
    a = 1
    b = 2
    c = 3
    !write(*, fmt=* ) a, b, c
    write(*, fmt=io_three_int ) a, b, c

    allocate(cvs_list(5))
    cvs_list = [1, 2, 3, 4, 5]
    write(*, '(*(I2,:,'','' ))') cvs_list

    i = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
    16, 17, 18, 19, 20, 21, 22, 23, 24]
    write(*, '(24(i4))') i
    write(*, '(i4)') i
end program IO_list