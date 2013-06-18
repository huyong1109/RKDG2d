module comm
use kinds, only: long_kind
use control, only: stepnow
use domain_mod, only: ele
use dimensions_mod, only: np,nc, nelem_x, nelem_y
implicit none 
private 

type :: neighbors
        sequence 
        integer :: 
public :: bndy
contains 
subroutine bndy(tn) 
    
    integer, intent(in) :: tn
    integer :: i,j, left, right, up, down
    !call stepnow(tn)

	! exchange Fu and u boundary 
       ! ele(i,j)%Fhat(1) = ele(left)%Fu(np)
       ! ele(left)%Fhat(2) = ele(i)%Fu(1)
       ! ele(i)%vhat(1) = ele(left)%v(np, tn)
       ! ele(left)%vhat(2) = ele(i)%v(1, tn)
       end do         
    end do 
end subroutine bndy 

end module comm 
