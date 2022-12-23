module mod_main_io
  use mod_heat_io
  implicit none
  integer :: nx = 30, ny = 10  
  logical :: print = .false.
  real(dk), parameter :: eps = 1.0e-8_dk
  integer, parameter, private :: strmx = 32, errmx=1024
!> auxiliary functions for boundary and initial conditions
!> as well as public module for entities from mod_heat
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
  subroutine setup()
    use ftn_getopt
    integer :: i, ios, iu
    character(len=strmx) :: sw, sw_val, fmtstring
    character(len=errmx) :: err
    logical :: ex
    type(opt_t) :: options(5)

    namelist /heat_options/ nx, ny, chkp, print, chkp_iter

    if (command_argument_count() > 0) then
       write(*,*) 'Setup: processing command line.'
       options = optinit( [ 'nx   ','ny   ','print', 'chkp ', 'iter ' ], &
            [ 'integer', 'integer', 'logical', 'logical', 'integer' ] )
       call optarg(options)
       call optval(options(1), nx)
       call optval(options(2), ny)
       call optval(options(3), print)
       call optval(options(4), chkp)
       call optval(options(5), chkp_iter)
     else
       inquire(file=nml_file, exist=ex)
       if (ex) then
          write(*,'(''Setup: processing namelist options.'')')
          open(newunit=iu, file=nml_file, form='FORMATTED', action='READ')
          read(iu, nml=heat_options, iostat=ios, iomsg=err)
          if (ios /= 0) then
             write(*,*) 'failed with message: ',err
             stop 'Aborting program.'
          end if
          close(iu)
       else
          write(*,*) 'Setup: proceeding with default values.'
       end if
    end if
    if (chkp) then
       inquire(file=chkp_file, exist=ex)
       if (ex) then
          write(*,'(''Setup: Readjusting problem size from existing checkpoint.'')')
          open(newunit=iu, file=chkp_file, form='UNFORMATTED', action='READ')
          read(iu) nx, ny
          close(iu)
       end if
    end if
    write(*,'(''Running with following settings:'')')
    write(*,'(''Problem size : nx = '',i0, '' ny = '',i0)') nx, ny
    write(*,'(''Printout     : '',l1)') print
    write(*,'(''Checkpointing: chkp = '',l1, '' chkp_iter = '',i0)') &
         chkp, chkp_iter
  end subroutine setup
end module mod_main_io
program heat
  use mod_main_io
  use timer
  implicit none
  integer :: it, nit
  real(dk) :: dt, ti, mflops


  call setup()

  if (chkp) then
     nit = 3
  else
     nit = 10000000
  end if

  call heat_create(nx, ny)

  call heat_ival(fival)
  call heat_bval('N',fbval_top)
  call heat_bval('S',fbval_bottom)
  call heat_bval('W',fbval_sides)
  call heat_bval('E',fbval_sides)

  dt = 0.25_dk / real(max(nx, ny))**2
  ti = dwalltime()
  do it=1, nit
     if (heat_iter(dt,chkp_iter) < eps) exit
   end do
  ti = dwalltime() - ti

  call heat_print(print)
  mflops = 11.0_dk * real(nx-2,dk)*real(ny-2,dk)*real(num_iter) / 1.e6_dk
  write(*,fmt='(''Completed '',i0,'' iterations in '',f10.3,'' seconds.'')') &
       num_iter, ti
  write(*,fmt='(''FP performance of Jacobi kernel:'',f12.3,'' MFlop/s.'')') &
       mflops/ti
  call heat_destroy()
end program heat
