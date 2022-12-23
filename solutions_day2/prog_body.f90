program prog_body
  use mod_body
  use mod_charged_body
  implicit none
  type(body) :: ball
  type(charged_body) :: cball


  ball = create_body( mass=1.8, pos=[ 0.0, 0.0, 0.5 ], & 
         vel=[ 0.01, 4.0, 0.0 ] )
  call print_body(ball)
  
  cball = create_charged_body( mass=1.8, pos=[ 0.0, 0.0, 0.5 ], & 
       vel=[ 0.01, 4.0, 0.0 ], charge=0.001 )
  call print_charged_body(cball)

  ball = cball
  call print_charged_body(ball)
! could also call print_body in the line above ...

end program prog_body
