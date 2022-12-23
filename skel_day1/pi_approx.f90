program pi_approx
 implicit none
 integer, parameter :: lk = selected_int_kind(12)
 integer, parameter :: dk = selected_real_kind(13,100)
 integer(lk), parameter :: nsteps = 1000000000_lk
 integer(lk) :: i
 real(dk) :: pi, step, sum, x

 step = 1.0 / nsteps
 sum = 0.0
 do i = 1, nsteps
  x = (i - 0.5) * step
  sum = sum + 1.0 / (1.0 + x*x)
 enddo
 pi = 4.0 * step * sum
 write(*, '(a,f25.17)') 'approximation of pi is ', pi
end program

