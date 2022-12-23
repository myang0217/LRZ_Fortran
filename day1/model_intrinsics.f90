program model_intrinsics
  implicit none
  integer, parameter :: ik = selected_int_kind(6)
  integer, parameter :: lk = selected_int_kind(12)
  integer, parameter :: rk = kind(1.0)
  integer, parameter :: dk = kind(1.0d0)

  real(rk) :: xdef
  real(dk) :: xext

  intrinsic :: precision, digits, range, minexponent, maxexponent, radix, &
               huge, tiny


  write(*,*) '------------------------General model properties--------------------------'
  write(*,*) 'Precision of real: ', precision(xdef)
  write(*,*) 'Epsilon of real: ', epsilon(xdef)
  write(*,*) 'Binary digits of real: ', digits(xdef)
  write(*,*) 'Range of real: ', range(xdef)
  write(*,*) 'Radix of real: ', radix(xdef)
  write(*,*) 'Minimum exponent of real: ', minexponent(xdef)
  write(*,*) 'Maximum exponent of real: ', maxexponent(xdef)
  write(*,*) 'Largest default real: ', huge(xdef)
  write(*,*) 'Smallest positive default real (u in decimal representation): ', tiny(xdef)
  write(*,*) 'Precision of double: ', precision(xext)
  write(*,*) 'Epsilon of double: ', epsilon(xext)
  write(*,*) 'Binary digits of double: ', digits(xext)
  write(*,*) 'Range of double: ', range(xext)
  write(*,*) 'Radix of double: ', radix(xext)
  write(*,*) 'Minimum exponent of double: ', minexponent(xext)
  write(*,*) 'Maximum exponent of double: ', maxexponent(xext)
  write(*,*) 'Largest double precision real: ', huge(xext)
  write(*,*) 'Smallest positive double precision (u in decimal representation): ', tiny(xext)
  write(*,*) '------------------------Properties of integers----------------------------'

  write(*,*) 'Largest kind(',lk,') integer: ', huge(1_lk)
  write(*,*) 'Largest kind(',ik,') integer: ', huge(1_ik)
  write(*,*) '------------------------Inquiries on real numbers-------------------------'
  write(*,*) 'Default real spacing(0.35): ', spacing(0.35_rk)
  write(*,*) 'Default real rrspacing(0.35): ', rrspacing(0.35_rk)
  write(*,*) 'Default real nearest(0.35, -1.0): ', nearest(0.35_rk, -1.0_rk)
  write(*,*) 'Default double spacing(0.35): ', spacing(0.35_dk)
  write(*,*) 'Default double rrspacing(0.35): ', rrspacing(0.35_dk)
  write(*,*) 'Default double nearest(0.35, -1.0): ', nearest(0.35_dk, -1.0_dk)

end program
