module mod_main_ptr
  use mod_heat_ptr
  implicit none
  integer :: nx = 30, ny = 10
!  logical, parameter :: print = .true.
  logical :: print = .false.
  real(dk), parameter :: eps = 1.0e-8_dk
  integer, parameter, private :: strmx = 32, errmx=1024
contains
!> auxiliary functions for boundary and initial conditions
!> as well as public module for entities from mod_heat
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
end module mod_main_ptr
program heat_ptr
  use mod_main_ptr
  use timer
  use ftn_getopt
  implicit none
  real(dk) :: dt, ti, mflops
  type(opt_t) options(3)
  
  options = optinit( [ 'nx   ','ny   ','print' ], &
       [ 'integer', 'integer', 'logical' ] )
  call optarg(options)
  call optval(options(1), nx)
  call optval(options(2), ny)
  call optval(options(3), print)

  call heat_create(nx, ny)

  call heat_ival(fival)
  call heat_bval('N',fbval_top)
  call heat_bval('S',fbval_bottom)
  call heat_bval('W',fbval_sides)
  call heat_bval('E',fbval_sides)

  dt = 0.25_dk / real(max(nx, ny))**2
  ti = dwalltime()
  do
     if (heat_iter(dt,1) < eps) exit
     if (print .and. mod(num_iter,100) == 1) call heat_print()
  end do
  ti = dwalltime() - ti

  call heat_print()
  mflops = 11.0_dk * real(nx-2,dk)*real(ny-2,dk)*real(num_iter) / 1.e6_dk
  write(*,fmt='(''Problem size '',i0,'' by '',i0)') nx, ny
  write(*,fmt='(''Completed '',i0,'' iterations in '',f10.3,'' seconds.'')') &
       num_iter, ti
  write(*,fmt='(''FP performance of Jacobi kernel:'',f12.3,'' MFlop/s.'')') &
       mflops/ti
  call heat_destroy()
end program heat_ptr
