program character_entities
  implicit none

  integer, parameter :: ck = kind('A')
  character(len=10) :: fh
  character(len=30) :: thanks
  character(kind=ck, &
          len=10) :: fhk

  fh = 'Full House'
  write(*,*) fh
  
  fhk = ck_'Full House'
  write(*,*) 'character kind is ',ck
  write(*,*) fhk

  write(*,*) 'Substring 3:6 of fh     : ',fh(3:6)
  write(*,*) 'Substring 3:6 of literal: ','Full House'(3:6)

  thanks = '"Thanks", he said'
  write(*,*) thanks

  thanks = "'Thanks', he said"
  write(*,*) thanks

  thanks = 'It''s true'
  write(*,*) thanks
end program character_entities
