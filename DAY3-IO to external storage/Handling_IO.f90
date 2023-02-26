program read_data_from_file
    implicit none

    integer :: i, n
    logical :: file_exists, file_opened
    real :: x, y
    character(len=50) :: filename

    print *, "Enter the name of the data file:"
    read *, filename

    ! Inquire about the status of the file
    inquire(file=trim(filename), exist=file_exists)
    if (file_exists) then
      print *, "The file ", trim(filename), " exists."
    else
      print *, "The file ", trim(filename), " does not exist."
    end if

    ! Open the file for reading
    open(unit=10, file=trim(filename), status='old', &
         action='read', iostat=i)
    print *, "iostat:", i

    !Error handling
    if (i /= 0) then
        print *, "Error opening file"
        stop
    end if

    ! Read the data
    n = 0
    do
        read(10,*,iostat=i) x, y
        if (i /= 0) exit
        n = n + 1
        print *, "Data point", n, ": x =", x, ", y =", y
      end do
    ! Close the file
    close(10)

  end program read_data_from_file