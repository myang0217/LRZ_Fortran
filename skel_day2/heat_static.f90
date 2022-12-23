module mod_main_static
  use mod_heat_static
!> auxiliary functions for boundary and initial conditions
!> as well as public module for entities from mod_heat
  implicit none
contains
! FIXME: write implementations of the functions needed to initialize the fields
end module mod_main_static
program heat_static
  use mod_main_static
  use timer
  implicit none
  logical, parameter :: print = .true.
!  logical, parameter :: print = .false.
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
! FIXME: write the iteration loop body with suitable exit condition
  end do
  ti = dwalltime() - ti

  call heat_print()
  mflops = 11.0_dk * real(ndim-2,dk)*real(ndim-2,dk)*real(num_iter) / 1.e6_dk
  write(*,fmt='(''Completed '',i0,'' iterations in '',f10.3,'' seconds.'')') &
       num_iter, ti
  write(*,fmt='(''FP performance of Jacobi kernel:'',f12.3,'' MFlop/s.'')') &
       mflops/ti
end program heat_static
