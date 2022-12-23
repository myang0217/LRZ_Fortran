program triads_dynamic
  use mod_triads
  implicit none
  integer, parameter :: maxsize = 17000000, ncases=7
  ! target execution time 
  double precision, parameter :: dintv = 0.1d0
  external :: dummy

  integer :: case, niter(ncases), nsize, it, i, stride
  double precision :: dt(ncases), operations(ncases), mflops(ncases)

  double precision, allocatable :: a(:), b(:), c(:), d(:)
  double precision, pointer, contiguous :: ap(:), bp(:), cp(:), dp(:)

  nsize = 64
  niter = 5000000
  stride = 1
  
  write(*, '('' Length  MFlop/s:1          2          3          4          5          6          7 '')')
  do
     !
     ! vector length
     allocate(a(nsize), b(nsize), c(nsize), d(nsize))
     call array_init(a)
     call array_init(b)
     call array_init(c)
     call array_init(d)

     !
     ! baseline
     case = 1
     dt(case) = dwalltime()
     do it=1, niter(case)
        do i=1, nsize
           d(i) = c(i) + a(i)*b(i)
        end do
        call dummy(d, a)
     end do
     dt(case) = dwalltime() - dt(case)

     !
     ! array syntax
     case = 2
     dt(case) = dwalltime()
     do it=1, niter(case)
        d(:) = c(:) + a(:)*b(:)
        call dummy(d, a)
     end do
     dt(case) = dwalltime() - dt(case)

     !
     ! assumed shape
     case = 3
     call assumed_shape_array(a, b, c, d, dt(case), niter(case))

     !
     ! contiguous assumed shape
     case = 4
     call contig_assumed_shape_array(a, b, c, d, dt(case), niter(case))

     deallocate(a, b, c, d)
     allocate(ap(nsize), bp(nsize), cp(nsize), dp(nsize))
     call array_init(ap)
     call array_init(bp)
     call array_init(cp)
     call array_init(dp)

     !
     ! pointer, intent(in)
     case = 5
     call pointer_intent_in(ap, bp, cp, dp, dt(case), niter(case))

     !
     ! pointer, contiguous
     case = 6
     call contig_pointer(ap, bp, cp, dp, dt(case), niter(case))

     !
     ! pointer, intent(inout)
     case = 7
     call pointer_intent_inout(ap, bp, cp, dp, dt(case), niter(case))

     ! evaluate results
     deallocate(ap, bp, cp, dp)
     operations = 2.0d0 * real(niter,kind(1.0d0)) * real(nsize,kind(1.0d0)) / real(stride,kind(1.0d0))
     mflops = operations / (dt * 1.0d6)
     write(*, '(i8,2x,7(f10.2,1x))') nsize,mflops
     
     !
     ! next iteration
!     write(*,*) 'input: ', niter(1), dt(1), niter(1) * dintv / dt(1)
     niter = int(real(niter,kind(1.0d0)) * dintv / dt)
     nsize = int(real(nsize,kind(1.0d0)) * 1.2d0)
!     write(*,'(''next size: '',i0,'' iteration counts: '',7(i0,x))') nsize, niter
     if (nsize > maxsize) exit
  end do
     
  
end program triads_dynamic
  
