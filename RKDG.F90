module RKDG
use kinds, only: long_kind
use control, only: dt, nstep, stepnow,stepnext, nextstep, Ffun
use element_mod, only: elem_t, timetemp, spctral_phy
use domain_mod, only: ele, nelem_local
use dimensions_mod, only:  np,nc
use dg_advance, only: compute_rhs
use comm, only: bndy 
implicit none 
private 
    public :: SSP_RK
contains
SUBROUTINE SSP_RK
    ! set boundary 
    ! use periodic boundary 
    integer :: i, ns ,tn, tnext
    real(kind=long_kind), dimension(nc) :: vrhs
    real(kind=long_kind), dimension(nc) :: vsptemp
    real(kind=long_kind), dimension(2) :: Flimit
    real(kind=long_kind),parameter :: c13 = 1.0/3.0
    type(elem_t) :: etemp
     
    write(*,*) "RKDG "

    do ns = 1, nstep
	call stepnow(tn)
	call stepnext(tnext)
	write(*,*) "stepnow = ",tn, "dt = ", dt, "dx = ", ele(1)%dx

	! SSP-RK step 1
	call set_boudry(tn)
	do i = 1, nelem_local
	    call compute_rhs(ele(i)%Fu, np, ele(i)%Fstar, vrhs, nc)
	   ! if (i ==nelem_local/2) then
	   !     write(*,*) "vsp begin 1 +++++++++++++++" 
	   !     write(*,*) " ------- v", tn, " --------"
	   !     write(*,*) ele(i)%v(:,tn)
	   !     write(*,*) " ------- 0vsp", tn, " --------"
	   ! 	write(*,*) ele(i)%vsp(:,tn)
	   ! endif 
	    ele(i)%vsp(:,timetemp) = ele(i)%vsp(:,tn) + dt/ele(i)%dx*vrhs(:)
	    !write(*,*) "================== vsp-> v ================="
	    !if (i ==nelem_local/2) then
	    !    write(*,*) "RK 1 vsp", timetemp," ---------------"
	    !    write(*,*)  ele(i)%vsp(:,timetemp)
	    !    write(*,*) " ------- 1vsp", tn," --------"
	    !    write(*,*) ele(i)%vsp(:,tn)
	    !endif
	    call spctral_phy( ele(i)%v(:,timetemp), np, ele(i)%vsp(:,timetemp), nc)
	   ! if (i ==nelem_local/2) then
	   !     write(*,*) " ------- v", timetemp," --------"
	   !     write(*,*) ele(i)%v(:,timetemp)
	   !     write(*,*) " ------- 2vsp", tn," --------"
	   !     write(*,*) ele(i)%vsp(:,tn)
	   ! endif
	end do 
	! SSP-RK step 2
	call set_boudry(timetemp)
	do i = 1, nelem_local
	    call compute_rhs(ele(i)%Fu, np, ele(i)%Fstar,vrhs, nc)

	   ! if (i ==nelem_local/2) then
	   !     write(*,*) "vsp begin 2 ", timetemp, " +++++++++++++++" 
	   !     write(*,*) ele(i)%vsp(:,timetemp)
	   !     write(*,*) " ------- vsp " , tn, "--------"
	   !     write(*,*) ele(i)%vsp(:,tn)
	   ! endif

	    ele(i)%vsp(:,timetemp) = 0.75*ele(i)%vsp(:,tn) + 0.25*(ele(i)%vsp(:,timetemp)+dt/ele(i)%dx*vrhs(:))
	   ! if (i ==nelem_local/2) then
	   !     write(*,*) "RK 2 ---------------"
	   !     write(*,*) " ------- vsp --------"
	   !     write(*,*)  ele(i)%vsp(:,tnext)
	   ! endif
	    call spctral_phy( ele(i)%v(:,timetemp), np, ele(i)%vsp(:,timetemp), nc)
	   ! if (i ==nelem_local/2) then
	   !     write(*,*) " ------- v --------"
	   !     write(*,*) ele(i)%v(:,tnext)
	   ! endif
	end do 
	! SSP-RK step 3
	call set_boudry(timetemp)
	do i = 1, nelem_local
	    call compute_rhs(ele(i)%Fu, np, ele(i)%Fstar, vrhs, nc)

	   ! if (i ==nelem_local/2) then
	   !     write(*,*) "vsp begin 3 +++++++++++++++" 
	   !     write(*,*) ele(i)%vsp(:,timetemp)
	   ! endif 

	    ele(i)%vsp(:,tnext) = c13*ele(i)%vsp(:,tn) + (1.0-c13)*(ele(i)%vsp(:,timetemp)+dt/ele(i)%dx*vrhs(:))
	    if (i ==nelem_local/2) then
		write(*,*) "RK 3 ---------------"
		write(*,*)  ele(i)%vsp(:,tnext)
	    endif 
	    call spctral_phy( ele(i)%v(:,tnext), np, ele(i)%vsp(:,tnext), nc)
	end do 

    	call nextstep 
    end do 
end subroutine SSP_RK

subroutine set_boudry(tn)
    real(kind=long_kind), dimension(2):: amax ! for linear problem
    integer, intent(in) :: tn
    integer :: i,j, left
    
    !write(*,*) "set boudary F(u) on step", tn
    !write(*,*) "========================================"
    !write(*,*) "	    F(u)	 " 
    do i = 1, nelem_local
        !F = element%v(:,tn)
        !write(*,*)  ele(i)%v(:,tn)
        call Ffun(ele(i)%v(:,tn),ele(i)%Fu(:),np)
!	do j = 1, np
!	    write(*,"(5X, E11.5\)")  ele(i)%Fu(j)
!	end do 
!	write(*,*) " " 
    end do 
    call bndy(tn)

    do i = 1, nelem_local 
	!write(*,*) "Fhat :", ele(i)%Fhat
	call getamax(ele(i), amax, tn)
        ele(i)%Fstar(1) = 0.5*(ele(i)%Fu(1) +ele(i)%Fhat(1) &
            & - amax(1)*(ele(i)%v(1,tn)-ele(i)%vhat(1)))
        ele(i)%Fstar(2) = 0.5*(ele(i)%Fu(np) +ele(i)%Fhat(2) &
            & - amax(2)*(-ele(i)%v(np,tn)+ele(i)%vhat(2)))
	!write(*,*) "Fstar :", ele(i)%Fstar
        
    end do 
!    write(*,*) "========================================"
end subroutine set_boudry

! miscallous function 
subroutine getamax(element, alpha, tn)
    use control, only : fexp
    type(elem_t), intent(in) :: element
    integer, intent(in) :: tn
    real(kind=long_kind), dimension(2) :: alpha
    select case(fexp)
    case (1)
	alpha(1) = 1.0
	alpha(2) = 1.0
    case (2) 
	alpha(1) = max(abs(element%v(1,tn)), abs(element%vhat(1)))
	alpha(2) = max(abs(element%v(np,tn)), abs(element%vhat(2)))
    case default 
	write(*,*) "fexp should be 1 or 2"
    end select	


end subroutine getamax




end module RKDG
