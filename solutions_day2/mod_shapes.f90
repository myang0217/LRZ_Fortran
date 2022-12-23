module mod_shapes
  implicit none
  private
  public :: create_shape, query_shape, area_shape
  integer, parameter :: tag_len = 10, dim = 2, npar = 3, num_shapes = 5
  real, parameter :: pi = 3.1415926535
  character(len=tag_len) :: register(num_shapes) = &
       [ 'undefined ', 'circle    ', 'triangle  ', &
          'rectangle ', 'square    ' ]

  type, public :: shape
     private
     character(len=tag_len) :: stype = 'undefined '
     real :: params(npar) = 0.0
  end type shape

  type(shape), public, protected :: unit_circle = &
               shape('circle',  [ 1.0, 0.0, 0.0 ] )
  type(shape), public, protected :: unit_triangle = &
               shape('triangle', [ 1.0, 1.0, pi/2 ] )
  type(shape), public, protected :: unit_square = &
               shape('square', [ 1.0, 0.0, 0.0 ] )
contains
  subroutine create_shape(this, stype, params)
    type(shape), intent(out) :: this
    character(len=*), intent(in) :: stype
    real, intent(in) :: params(:)
!
    integer :: i
!
    do i=2, num_shapes
       if (stype == register(i)) then
          this%stype = register(i)
          select case (i)
          case (2) ! circle with radius params(1)
             this%params(1) = params(1)
          case (3) ! triangle with sides params(1:2) and enclosed angle params(3)
             this%params(1:3) = params(1:3)
          case (4) ! rectangle with sides params(1:2)
             this%params(1:2) = params(1:2)
          case (5) ! square with side params(1)
             this%params(1) = params(1)
          case default
             stop 'mod_shape::create_shape: internal error.'
          end select
          return
       end if
    end do
  end subroutine create_shape
  function query_shape(this) result(s)
    type(shape), intent(in) :: this
    character(len=len(this%stype)) :: s
    s = this%stype
  end function query_shape
  real function area_shape(this)
    type(shape), intent(in) :: this
    integer :: i
    area_shape = 0.0
    do i=2, num_shapes
       if (this%stype == register(i)) then
          select case (i)
          case (2)
             area_shape = this%params(1)**2 * pi
          case (3)
             area_shape = 0.5 * this%params(1) * this%params(2) * sin(this%params(3))
          case (4)
             area_shape = this%params(1) * this%params(2)
          case (5)
             area_shape = this%params(1)**2 
          case default
             stop 'mod_shape::area_shape: internal error.'
          end select
          return
       end if
    end do
  end function area_shape
end module mod_shapes
program test_mod_shapes
  use mod_shapes, local_shape => shape
  implicit none
  real :: params(3)
  type(local_shape) :: my_shapes(5)

  call create_shape(my_shapes(2), 'circle', [ 1.0/sqrt(4.0*atan(1.0)) ])
  call create_shape(my_shapes(3), 'triangle', [ 1.0, 2.0, 2.0*atan(1.0) ])
  call create_shape(my_shapes(4), 'rectangle', [ 2.0, 0.5 ])
  call create_shape(my_shapes(5), 'square', [ 2.0 ])

  write(*,fmt='('' Shape 1 is '',a,''   (should be undefined)'')') query_shape(my_shapes(1))
  write(*,fmt='('' Shape 2 is a '',a,'' (should be circle)'')') query_shape(my_shapes(2))
  write(*,fmt='('' Shape 3 is a '',a,'' (should be triangle)'')') query_shape(my_shapes(3))
  write(*,fmt='('' Shape 4 is a '',a,'' (should be rectangle)'')') query_shape(my_shapes(4))
  write(*,fmt='('' Shape 5 is a '',a,'' (should be square)'')') query_shape(my_shapes(5))

  write(*,fmt='('' Shape 1 has area '',f4.1,''   (should be 0.0)'')') area_shape(my_shapes(1))
  write(*,fmt='('' Shape 2 has area '',f4.1,''   (should be 1.0)'')') area_shape(my_shapes(2))
  write(*,fmt='('' Shape 3 has area '',f4.1,''   (should be 1.0)'')') area_shape(my_shapes(3))
  write(*,fmt='('' Shape 4 has area '',f4.1,''   (should be 1.0)'')') area_shape(my_shapes(4))
  write(*,fmt='('' Shape 5 has area '',f4.1,''   (should be 4.0)'')') area_shape(my_shapes(5))


  write(*,fmt='('' Unit circle is a '',a,'' with area '',f12.7)') &
                   query_shape(unit_circle), area_shape(unit_circle)
  write(*,fmt='('' Unit triangle is a '',a,'' with area '',f12.7)') &
                   query_shape(unit_triangle), area_shape(unit_triangle)
  write(*,fmt='('' Unit square is a '',a,'' with area '',f12.7)') &
                   query_shape(unit_square), area_shape(unit_square)
!  the following should be rejected by the compiler if uncommented
!  call create_shape(unit_square, 'square', [ 2.0 ])


end program test_mod_shapes
