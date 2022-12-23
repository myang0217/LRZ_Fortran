program body_structs
  use mod_body_bm
  implicit none

  integer, parameter :: ndim = 10, ddim = 12
  
  type( body_p (ntraj=ndim) ) :: traj_ndim
  type( body_p ) :: traj_1
  type( body_p (k=kind(1.0d0), ntraj=ndim) ) :: dptraj_ndim
  type( body_p (ntraj=:) ), allocatable :: dyn_traj

  allocate( body_p(ntraj=ddim) :: dyn_traj )

  write(*,fmt='(''traj_ndim   parameter values: '',2i6)') traj_ndim%k,  traj_ndim%ntraj
  write(*,fmt='(''traj_1      parameter values: '',2i6)') traj_1%k,  traj_1%ntraj
  write(*,fmt='(''dptraj_ndim parameter values: '',2i6)') dptraj_ndim%k,  dptraj_ndim%ntraj
  write(*,fmt='(''dyn_traj    parameter values: '',2i6)') dyn_traj%k, dyn_traj%ntraj

  write(*,fmt='(''dyn_traj    shape of vel component: '',2i6)') shape(dyn_traj%vel)
 

end program body_structs

