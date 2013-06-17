
module dimensions_mod
use quadrature_mod,only: quadrature_t, get_gll
use physical_constants, only: DD_PI

  implicit none
! set MAX number of tracers.  actual number of tracers is a run time argument  
  integer, parameter, public :: np = 4
  integer, parameter, public :: nc = 4
  real, parameter, public  :: xbound_beg = -DD_PI
  real, parameter, public  :: xbound_end =  DD_PI
  real, parameter, public  :: ybound_beg = -DD_PI
  real, parameter, public  :: ybound_end =  DD_PI


  integer, public :: nelem_x  = 80      ! total number of elements
  integer, public :: nelem_y  = 80      ! total number of elements
  
  type(quadrature_t), public :: gll

  private 

  public :: init_dimension
contains
  subroutine init_dimension
    call get_gll(np, nc, gll)
    
   end subroutine init_dimension
	
end module dimensions_mod



