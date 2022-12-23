module libm_interfaces
 implicit none
 interface
   real(c_float) function lgammaf_r(x, is) BIND(C)
     use, intrinsic :: iso_c_binding
     implicit none
     real(c_float), value :: x
     integer(c_int) :: is
   end function
 end interface
end module
program calc_gamma
 use, intrinsic :: iso_c_binding
 use libm_interfaces
 implicit none
 integer, parameter :: md = 20
 real(c_float), parameter :: x0 = -0.5, dx = 0.33
 real(c_float) :: x, y
 integer(c_int) :: isign
 integer :: i
 x = x0
 do i = 1, md
    y = lgammaf_r(x, isign) 
    write(*, *) 'x = ',x, ' lgamma  = ',y, ' sign of gamma = ',isign
    x = x + dx
 end do 
end program
