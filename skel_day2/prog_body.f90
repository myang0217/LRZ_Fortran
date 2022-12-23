program prog_body
  use mod_body
  implicit none
  type(body) :: ball


  ball = body( mass=1.8, pos=[ 0.0, 0.0, 0.5 ], & 
         vel=[ 0.01, 4.0, 0.0 ] )
  write(*,fmt='(''body with mass '',e9.2)') ball%mass
  write(*,fmt='(''          position: '',3(e9.2))') ball%pos
  write(*,fmt='(''          velocity: '',3(e9.2))') ball%vel

end program prog_body
