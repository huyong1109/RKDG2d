
module dimensions_mod
use quadrature_mod,only: quadrature_t, get_gll

  implicit none
! set MAX number of tracers.  actual number of tracers is a run time argument  
  integer, parameter, public :: np = 4
  integer, parameter, public :: nc = 4
  real, parameter, public  :: a = 0.0
  real, parameter, public  :: b = 2.0


  integer, public :: nelem  = 80      ! total number of elements
  
  type(quadrature_t), public :: gll

  private 

  public :: init_dimension
contains
  subroutine init_dimension
    call get_gll(np, nc, gll)
    
   end subroutine init_dimension
	
end module dimensions_mod



