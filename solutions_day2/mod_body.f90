module mod_body
  implicit none
  private
  public :: body, create_body, print_body, set_charge

  type :: body
     private
     real :: mass
     real :: pos(3)
     real :: vel(3)
     real :: charge
  end type body
contains
  type(body) function create_body(mass, pos, vel)
    real, intent(in) :: mass, pos(3), vel(3)
    create_body = body(mass, pos, vel, 0.0)
  end function create_body
  subroutine set_charge(this, charge)
    type(body), intent(inout) :: this
    real, intent(in) :: charge
    this%charge = charge
  end subroutine set_charge
  subroutine print_body(this)
    type(body), intent(in) :: this
    write(*,fmt='(''body with mass '',e9.2)') this%mass
    if (this%charge /= 0.0) then
       write(*,fmt='(''body with charge '',e9.2)') this%charge
    end if
    write(*,fmt='(''          position: '',3(e9.2))') this%pos
    write(*,fmt='(''          velocity: '',3(e9.2))') this%vel
  end subroutine print_body
end module mod_body

