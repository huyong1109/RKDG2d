
module kinds
  implicit none
  private
!
!  most floating point variables should be of type real_kind = real*8
!  For higher precision, we also have quad_kind = real*16, but this
!  is only supported on IBM systems
! 

  integer (kind=4), public, parameter ::  &
  int_kind     = 4,                       &
  long_kind    = 8,                       &
  log_kind     = 4,                       &
  real_kind    = 8,                       &
  iulog        = 6,                       &   ! stderr file handle
  longdouble_kind = 16                

end module kinds

