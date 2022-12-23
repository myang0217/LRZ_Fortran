program triads
  use mod_triads
  implicit none
  integer, parameter :: maxsize = 17000000, ncases=5
  ! target execution time 
  double precision, parameter :: dintv = 0.25d0
  external :: dummy

  integer :: case, niter(ncases), nsize, it, i, stride
  double precision :: dt(ncases), operations(ncases), mflops(ncases)

  real(rk), allocatable :: ar(:), br(:), cr(:), dr(:)
  real(dk), allocatable :: ad(:), bd(:), cd(:), dd(:)
  integer(ik), allocatable :: ai(:), bi(:), ci(:), di(:)
  integer(lk), allocatable :: al(:), bl(:), cl(:), dl(:)
  complex(dk), allocatable :: az(:), bz(:), cz(:), dz(:)


  nsize = 64
  niter = 20000000
  stride = 1
  
  write(*, '('' Length  MFlop/s:1          2          3          4          5  '')')
  !
  ! vector length
  do
     !
     ! default real
     allocate(ar(nsize), br(nsize), cr(nsize), dr(nsize))
     call array_init_rk(ar)
     call array_init_rk(br)
     call array_init_rk(cr)
     call array_init_rk(dr)

     case = 1
     call eval_triads_rk(ar, br, cr, dr, nsize, dt(case), niter(case))

     deallocate(ar, br, cr, dr)

     !
     ! extended real
     allocate(ad(nsize), bd(nsize), cd(nsize), dd(nsize))
     call array_init_dk(ad)
     call array_init_dk(bd)
     call array_init_dk(cd)
     call array_init_dk(dd)

     case = 2
     call eval_triads_dk(ad, bd, cd, dd, nsize, dt(case), niter(case))

     deallocate(ad, bd, cd, dd)

     !
     ! default integer
     allocate(ai(nsize), bi(nsize), ci(nsize), di(nsize))
     call array_init_ik(ai)
     call array_init_ik(bi)
     call array_init_ik(ci)
     call array_init_ik(di)

     case = 3
     call eval_triads_ik(ai, bi, ci, di, nsize, dt(case), niter(case))

     deallocate(ai, bi, ci, di)

     !
     ! extended integer
     allocate(al(nsize), bl(nsize), cl(nsize), dl(nsize))
     call array_init_lk(al)
     call array_init_lk(bl)
     call array_init_lk(cl)
     call array_init_lk(dl)

     case = 4
     call eval_triads_lk(al, bl, cl, dl, nsize, dt(case), niter(case))

     deallocate(al, bl, cl, dl)

     !
     ! extended complex
     allocate(az(nsize), bz(nsize), cz(nsize), dz(nsize))
     call array_init_cz(az)
     call array_init_cz(bz)
     call array_init_cz(cz)
     call array_init_cz(dz)

     case = 5
     call eval_triads_cz(az, bz, cz, dz, nsize, dt(case), niter(case))

     deallocate(az, bz, cz, dz)

     !
     ! evaluate results
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
     
  
end program triads
  
