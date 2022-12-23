program body_bm
  use mod_body_bm
  use timer
  implicit none

  integer, parameter :: maxsize = 400000, ncases = 2
  ! target execution time 
  double precision, parameter :: dintv = 0.1d0
  
  type(body_s), allocatable :: traj_s(:)
  type(body_p(ntraj=:)), allocatable :: traj_p
  real, allocatable :: dp(:,:), dpt(:,:)

  double precision :: dt(ncases), operations(ncases), mflops(ncases)
  integer :: it, nsize, niter(ncases), case
 
  nsize = 64
  niter = 20000000
  dp = spread([1.e-9, 1.e-9, 1.e-9],dim=1,ncopies=maxsize)
  dpt = spread([1.e-9, 1.e-9, 1.e-9],dim=2,ncopies=maxsize)

  write(*, '('' Length  MFlop/s:1          2      '')')

  do
     allocate(traj_s(nsize))
     traj_s(:) = body_s('MKS ',1.0, [0.0,0.0,0.0], [0.0,0.0,0.0])
     !
     ! AoS
     case = 1
     dt(case) = dwalltime()
     do it=1, niter(case)
        call kick_s(traj_s,dpt)
     end do
     dt(case) = dwalltime() - dt(case)

     deallocate(traj_s)
     allocate(body_p(ntraj=nsize) :: traj_p)

     !
     ! SoA
     case = 2
     dt(case) = dwalltime()
     do it=1, niter(case)
!         write(*,*) 'iteration number ',it
        call kick_p(traj_p,dp)
     end do
     dt(case) = dwalltime() - dt(case)

     deallocate(traj_p)

     !
     ! evaluate results
     operations = 2.0d0 * real(niter,kind(1.0d0)) * real(nsize,kind(1.0d0)) 
     mflops = operations / (dt * 1.0d6)
     write(*, '(i8,2x,7(f10.2,1x))') nsize,mflops
     !
     ! next iteration
     niter = int(real(niter,kind(1.0d0)) * dintv / dt)
     nsize = int(real(nsize,kind(1.0d0)) * 1.2d0)
!     write(*,'(''next size: '',i0,'' iteration counts: '',7(i0,x))') nsize, niter
     if (nsize > maxsize) exit
  end do

end program body_bm
