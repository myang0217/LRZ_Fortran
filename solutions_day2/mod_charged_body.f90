module mod_charged_body
  use mod_body, only : charged_body => body, print_charged_body => print_body, &
       create_body, set_charge
  implicit none
  private
  public :: charged_body, print_charged_body, create_charged_body
contains
  type(charged_body) function create_charged_body(mass, pos, vel, charge)
    real, intent(in) :: mass, pos(3), vel(3), charge
    create_charged_body = create_body(mass, pos, vel)
    call set_charge(create_charged_body, charge)
  end function create_charged_body
end module mod_charged_body
 
