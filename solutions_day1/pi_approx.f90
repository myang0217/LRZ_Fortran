program pi_approx
 implicit none
 integer, parameter :: lk = selected_int_kind(12)
 integer, parameter :: dk = selected_real_kind(13,100)
 integer(lk), parameter :: nsteps = 1000000000_lk
 integer(lk) :: i
 real(dk) :: pi, step, sum, x

 step = 1.0_dk / real(nsteps,dk)
 sum = 0.0_dk
 do i = 1, nsteps
  x = (i - 0.5_dk) * step
  sum = sum + 1.0_dk / (1.0_dk + x*x)
 enddo
 pi = 4.0_dk * step * sum
 write(*, '(a,f20.17)') 'approximation of pi is ', pi
end program

