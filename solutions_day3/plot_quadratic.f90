program plot_quadratic
  use mod_solver
  use plplot
  use, intrinsic :: iso_fortran_env
  implicit none
  integer, parameter :: dk = plflt, method = 0
  character(len=4), parameter :: device = 'xwin'
!  character(len=4), parameter :: device = 'png'
  integer :: i, istatus, npoints, nroots
!
  real(dk), allocatable :: a(:), r1(:), r2(:)
  real, allocatable :: da(:), dr1(:), dr2(:)
!
! read values and allocate working space
  write(*,*) 'Enter the number of parameter values (suggest 100):'
  read(*,*)  npoints
  allocate(a(npoints), r1(npoints), r2(npoints), &
       da(npoints), dr1(npoints), dr2(npoints), stat=istatus)
  if (istatus /= 0) stop 'Abort due to failed allocation'
!
  call plparseopts(PL_PARSE_FULL)
  call plsetopt('dev',device)
!  call plsetopt('o','quadratic.png')
  call plinit()
! set up properties of page
  call plfont(2)
  call plcol0(15)
  call plenv(-2.0_plflt, 1.2_plflt, -7.0_plflt, 7.0_plflt, 0, 1)

! generate data
  da = (/ ((-2.0d0 + real(i - 1)/real(npoints - 1) * 3.0d0), i=1, npoints) /)
  a = da
  if (method == 0) then
     r1 = roots_quad(a, (/ (1.0_plflt, i=1, npoints) /))
     r2 = roots_quad(a, (/ (-1.0_plflt, i=1, npoints) /))
  else
     do i = 1, npoints
        call solve_quadratic(da(i), real(2.0d0 - da(i)*sqrt(abs(da(i)))), &
             real(2.5d-1*da(i)**2), nroots, dr1(i), dr2(i))
     end do
     r1 = dr1; r2 = dr2
  end if

! logarithmic plot
  call plcol0(2)
  call plline(a, r1)
  call plcol0(3)
  call plline(a, r2)
! legend

  call plcol0(15)
  call plmtex('t', 2.0_plflt, 0.5_plflt, 0.5_plflt, &
                  'Solutions of single-parameter quadratic')
  call plmtex('l', 5.0_plflt, 0.5_plflt, 0.5_plflt, 'Root value')
  call plmtex('t', -29.0_plflt, 0.5_plflt, 0.5_plflt, &
                  'Parameter value')
  call plend()
contains
  elemental real(dk) function roots_quad(a,sign)
    real(dk), intent(in) :: a, sign
    real(dk) :: dis, rt, dd
    rt = a * sqrt(abs(a))
    dd = 0.0_dk
    if (a < 0.0_dk) dd = 0.5_dk * abs(a)**3
    dis = 2.0_dk * sqrt(1.0_dk - rt + dd)

    roots_quad = (rt - 2.0_dk + sign * dis)/(2.0_dk * a)
  end function roots_quad
end program plot_quadratic
