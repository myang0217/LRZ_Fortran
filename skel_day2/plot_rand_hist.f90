!
! program that plots histograms for the deviation of the distribution of
! 100000 integer numbers in the range [-4, 7] from  the mean.
!
! NOTE: this program uses FGSL/GSL to store histogram data
!
program plot_histogram
  use plplot
  use fgsl
  implicit none
  integer, parameter :: imin = -4, imax = 7
  integer, parameter :: num_rand = 100000
  logical, parameter :: debug = .false.
  integer :: i, n, status, irand(num_rand)
  character(len=20) :: string
!
  type(fgsl_histogram) :: h
!
  real(plflt) :: y0(imin:imax)

! set up histogram
  h = fgsl_histogram_alloc(int(imax - imin + 1, fgsl_size_t))
  status = fgsl_histogram_set_ranges_uniform(h, real(imin,fgsl_double)-0.5_fgsl_double, &
       real(imax,fgsl_double)+0.5_fgsl_double)
  irand = -3
  do n = 1, num_rand
     status = fgsl_histogram_increment(h, real(irand(n),fgsl_double))
  end do
  do i = imin, imax
     y0(i) = real(fgsl_histogram_get(h, int(i - imin, fgsl_size_t)),plflt)
!     write(*, *) 'i = ',i,' y0 = ',y0(i)
  end do
  y0 = y0 - fgsl_histogram_sum(h) / real(imax - imin + 1)
  if (debug) then
     write(*, *) 'Sum is ', sum(y0), 'mean is ',fgsl_histogram_sum(h) / real(imax - imin + 1)
     write(*,*) 'y0 = ',y0
  end if

! initialize plotting
  call plparseopts(PL_PARSE_FULL)
  
  call plinit()
  
  call pladv(0)
  call plvsta()
  call plwind( 0._plflt, real(imax-imin+1,plflt), -500._plflt, +500._plflt )
  call plbox( 'bc', 1._plflt, 0, 'bcnv', 10._plflt, 0 )
  call plcol0(2)
  call pllab( 'value', 'deviation from mean', &
       'Integer random numbers' )

  do i = imin, imax
     call plcol0(i - imin + 1)
     call plpsty(0)
     call plfbox( real(i-imin,plflt), y0(i) )
     write (string, '(i8)') nint(y0(i))
     string = adjustl(string)
     call plptex( real(i - imin,plflt)+0.5_plflt, y0(i)+sign(60.0,y0(i)), &
          1._plflt, 0._plflt, 0.5_plflt, string )
     write (string, '(i8)') i 
     string = adjustl(string)
     call plmtex( 'b', 1._plflt, (i+1)*0.083_plflt+0.29_plflt, &
          0.5_plflt, string )
  end do

  call plend()
contains
  subroutine plfbox(x0, y0)
    use plplot
    implicit none

    real(kind=plflt) x0, y0, x(4), y(4)

    x(1) = x0
    y(1) = 0._plflt
    x(2) = x0
    y(2) = y0
    x(3) = x0+1._plflt
    y(3) = y0
    x(4) = x0+1._plflt
    y(4) = 0._plflt
    call plfill(x, y)
    call plcol0(1)
    call pllsty(1)
    call plline(x, y)
  end subroutine plfbox
end program
