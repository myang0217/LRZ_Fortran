module mod_main_static
  use mod_heat_static
!> auxiliary functions for boundary and initial conditions
!> as well as public module for entities from mod_heat
  implicit none
contains
  pure real(dk) function fival(x, y)
    real(dk), intent(in) :: x, y
    fival = 0._dk
  end function fival
  pure real(dk) function fbval_top(x)
    real(dk), intent(in) :: x
    fbval_top = 0.0_dk
  end function fbval_top
  pure real(dk) function fbval_bottom(x)
    real(dk), intent(in) :: x
    fbval_bottom = 1.0_dk
  end function fbval_bottom
  pure real(dk) function fbval_sides(x)
    real(dk), intent(in) :: x
    fbval_sides = x
  end function fbval_sides
end module mod_main_static
program heat_static
  use mod_main_static
  use timer
  implicit none
!  logical, parameter :: print = .true.
  logical, parameter :: print = .false.
  real(dk), parameter :: eps = 1.0e-8_dk
  real(dk) :: dt, ti, mflops

  call heat_ival(fival)
  call heat_bval('N',fbval_top)
  call heat_bval('S',fbval_bottom)
  call heat_bval('W',fbval_sides)
  call heat_bval('E',fbval_sides)

  dt = 0.25_dk / real(ndim)**2
  ti = dwalltime()
  do
     if (heat_iter(dt,1) < eps) exit
     if (print .and. mod(num_iter,100) == 1) call heat_print()
  end do
  ti = dwalltime() - ti

  call heat_print()
  mflops = 11.0_dk * real(ndim-2,dk)*real(ndim-2,dk)*real(num_iter) / 1.e6_dk
  write(*,fmt='(''Completed '',i0,'' iterations in '',f10.3,'' seconds.'')') &
       num_iter, ti
  write(*,fmt='(''FP performance of Jacobi kernel:'',f12.3,'' MFlop/s.'')') &
       mflops/ti
end program heat_static
