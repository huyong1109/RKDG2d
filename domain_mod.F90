
module domain_mod
use dimensions_mod, only :  nelem
use element_mod, only: elem_t
  implicit none
  private
  
  type, public :: domain1D_t
     sequence
     integer :: start                     ! domain start index
     integer :: end                       ! domain end index
  end type domain1D_t

  integer, public :: nelem_local
  type(elem_t),allocatable, public :: ele(:)
  type(domain1D_t), public :: domain


  public :: decompose, init_domain
contains 
function decompose(start,end) result(domain)

    integer, intent(in) :: start      ! starting index
    integer, intent(in) :: end        ! ending   index


    type (domain1D_t) :: domain

    ! Local stuff 
    domain%start = start
    domain%end = end

  end function decompose

  subroutine init_domain()

    domain = decompose(1, nelem)
    nelem_local = domain%end - domain%start +1  !for mpi
    write(*,*) nelem_local
    allocate(ele(nelem_local))

  end subroutine init_domain


end module domain_mod





