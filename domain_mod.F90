! for MPI task division
module domain_mod
use dimensions_mod, only :  nelem_x, nelem_y
use element_mod, only:  elem_t
  implicit none
  private
  ! types   
  type, public :: domain1D_t
     sequence
     integer :: start                     ! domain start index
     integer :: end                       ! domain end index
  end type domain1D_t

  type, public :: domain2D_t
     sequence
     type (domain1D_t) :: x
     type (domain1D_t) :: y
     integer :: nelem_lx, nelem_ly
  end type domain2D_t

  type, private :: neighbors 
      sequence 
      integer :: left, right, up, down
  end type neighbors
  ! variable   
  type(elem_t),allocatable, public :: ele(:,:)
  type(domain2D_t), public :: domain
  type(neighbors), public :: neigh

  public :: decompose, init_domain
contains 
  subroutine decompose(proc_x, proc_y, nproc, domain, neigh)
    
    ! input 
    integer, intent(in) :: proc_x, proc_y, nproc 
    type(domain2D_t), intent(out):: domain
    type(neighbors), intent(out) :: neigh
    
    ! local 

    integer :: lproc_x, lproc_y
    integer :: lelem_x, lelem_y
    

    lproc_x = mod(nproc, proc_x)
    lproc_y = nproc/proc_x

    ! set neighbours  
    if(lproc_x == 1 ) then 
        neigh%left  = nproc + proc_x-1
        neigh%right = nproc +1
    else if(lproc_x == proc_x) then
        neigh%left  = nproc -1
        neigh%right = nproc - proc_x +1
    else 
        neigh%left  = nproc-1
        neigh%right = nproc+1
    endif 
    if(lproc_y == 0) then 
        neigh%up    = nproc + proc_x
        neigh%down  = nproc + (proc_y-1)*proc_x
    else if (lproc_y == proc_y-1) then
        neigh%up    = nproc - (proc_y-1)*proc_x
        neigh%down  = nproc - proc_x

    else
        neigh%up    =  nproc + proc_x
        neigh%down  =  nproc - proc_x
    endif 
    write(*,*) "PROC id : ", nproc
    write(*,*) "  left = ", neigh%left, "  right = ",neigh%right  
    write(*,*) "  up   = ", neigh%up,   "  down  = ", neigh%down

    ! Local stuff , serial version 
    if (mod(nelem_x, proc_x) /= 0 .OR. mod(nelem_y, proc_y)  /= 0) then 
        write(*,*) " !!! Process number cannot divide element number"
    else 
        lelem_x = nelem_x/proc_x
        lelem_y = nelem_y/proc_y
        domain%x%start = lproc_x*lelem_x +1
        domain%x%end   = lproc_x*(lelem_x+1)
        domain%y%start = lproc_y*lelem_y +1
        domain%y%end   = lproc_y*(lelem_y+1)
        domain%nelem_lx = domain%x%end - domain%x%start +1  
        domain%nelem_ly = domain%y%end - domain%y%start +1  
        write(*,*) "PROC id : ", nproc, "  xs = ", domain%x%start, "  xe = ", domain%x%end
        write(*,*) "  ys = ", domain%y%start, "  ye = ", domain%y%end
    endif 
    return 
  end subroutine decompose

  subroutine init_domain(proc_x, proc_y)
    !for mpi
    integer, intent(in) :: proc_x, proc_y
    integer :: myid 
    
    ! serial 
    myid = 1
    
    call decompose(proc_x, proc_y, myid, domain, neigh)
    !write(*,*) nelem_local
    allocate(ele(domain%nelem_lx, domain%nelem_ly))

  end subroutine init_domain

  subroutine end_domain( )
    !for mpi
    deallocate(ele)

  end subroutine end_domain

end module domain_mod





