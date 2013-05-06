
!
! This module should 'use' only module 'kinds'
!
module physical_constants
  ! ------------------------------
  use kinds, only : real_kind, longdouble_kind
  implicit none

  private
  real (kind=real_kind), public, parameter :: DD_PI = 3.141592653589793238462643383279_real_kind
  real (kind=real_kind), public, parameter :: rearth       = 6.376D6         ! m
  real (kind=real_kind), public, parameter :: omega        = 7.292D-5        ! radians/s
  real (kind=real_kind), public, parameter :: g            = 9.80616D0       ! m s^-2

end module physical_constants
