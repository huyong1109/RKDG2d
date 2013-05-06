module dg_advance 
use kinds, only: long_kind
use control, only: dt, stepnow, stepnext
use dimensions_mod, only: np, nc, gll
use element_mod, only: line_int

implicit none 
private 


    public :: init_advance, compute_rhs

contains
subroutine init_advance
    !allocate(Fintegral(nc)) 
     
    
end subroutine init_advance
subroutine compute_rhs(Fu, np, Fhat, vrhs, nc)
    !
    integer, intent(in) :: np, nc
    real(kind=long_kind), dimension(np), intent(in):: Fu
    real(kind=long_kind), dimension(2), intent(in):: Fhat
    real(kind=long_kind), dimension(nc),intent(inout) :: vrhs
    ! local 
    real(kind=long_kind) :: Fintegral(nc)
    integer :: i, j
    real(kind=long_kind), dimension(2) :: fhattemp
    real(kind=long_kind), dimension(np):: FX

    Fintegral(1) = 0

    do i = 2, nc
	do j = 1,np
	    FX(j) = Fu(j)*gll%der(j,i)
	end do 
	call line_int(FX, Fintegral(i), np)
    end do 
    !write(*,*) "integral" 
    !write(*,*) Fintegral
    
    fhattemp(1) = Fhat(2)- Fhat(1)
    fhattemp(2) = Fhat(2)+ Fhat(1)
    do i = 1,nc, 2
	vrhs(i) =  (2*i-1)*(Fintegral(i) - fhattemp(1))
    end do 
    do i = 2,nc, 2
	vrhs(i) =  (2*i-1)*(Fintegral(i) - fhattemp(2))
    end do 
    !  
	
end subroutine compute_rhs



! Get   integral 
!   S v(x) *P'(k) dx
!


end module dg_advance 
    
