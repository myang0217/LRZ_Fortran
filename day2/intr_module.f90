program intr_module
!
! illustrates use of some features of the ISO Fortran intrinsic module
  use, intrinsic :: iso_fortran_env
  implicit none
  integer, parameter :: wp = real64, ik = int32, strmx=128

  real(kind=wp) :: x
  integer(kind=ik) :: i4

! the following is not yet universally supported. gcc 6.x and higher work.
  character(len=strmx), parameter :: version = compiler_version()

  write(*, *) 'Compiler version is ',version
end program

