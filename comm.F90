module comm
use kinds, only: long_kind
use control, only: stepnow
use domain_mod, only: ele
use dimensions_mod, only: np,nc, nelem
implicit none 
private 

public :: bndy
contains 
subroutine bndy(tn) 
    
    integer, intent(in) :: tn
    integer :: i, left
    !call stepnow(tn)
    do i = 1, nelem
	if(i == 1) then 
            left = nelem
        else 
            left = i -1
        endif 
	! exchange Fu and u boundary 
        ele(i)%Fhat(1) = ele(left)%Fu(np)
        ele(left)%Fhat(2) = ele(i)%Fu(1)
        ele(i)%vhat(1) = ele(left)%v(np, tn)
        ele(left)%vhat(2) = ele(i)%v(1, tn)
        
    end do 
end subroutine bndy 

end module comm 
