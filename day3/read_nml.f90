program read_nml
  implicit none
  real :: flour, breadcrumbs, salt, pepper
  integer :: apples, pears

  namelist /groceries/ flour, breadcrumbs, salt, pepper
  namelist /fruit/ pears, apples

  open(12, file='my_nml.dat', form='formatted', action='read')

  pepper = -1.2

  read(12, nml=groceries)
  read(12, nml=fruit)

  write(*, *) 'flour:', flour
  write(*, *) 'breadcrumbs:', breadcrumbs
  write(*, *) 'salt:', salt
  write(*, *) 'pepper:', pepper
  write(*, *) 
  write(*, *) 'pears:', pears
  write(*, *) 'apples:', apples
end program
