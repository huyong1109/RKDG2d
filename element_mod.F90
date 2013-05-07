
module element_mod
  ! ------------------------------
  use kinds, only : long_kind, int_kind
  ! ------------------------------
  use dimensions_mod, only : np, nc, gll
  !----------------------------
  implicit none
  private
  integer, public, parameter :: timelevels=2
  integer, public, parameter :: timetemp = timelevels+1


  
  type, public :: elem_t
!
!    note: variables (and sequence) must match that in prim_restart_mod.F90
!
! prognostic variables
!
     integer(kind=int_kind):: elem_id
     real (kind=long_kind) :: xa   ! 
     real (kind=long_kind) :: xb   ! 
     real (kind=long_kind) :: dx   ! 
     real (kind=long_kind) :: v(np,timelevels+1)   ! velocity         
     real (kind=long_kind) :: vsp(nc, timelevels+1)   ! spectral coefficient
     ! Flimit(1) = F+(x_a), Flimit(2) = F-(x_b)
     real (kind=long_kind) :: Fu(np)   
     ! Fhat(1) = F^(x_a) = 0.5*(F- + F+) - \alpha (U- +U+), Fhat(2) = F^(x_b)
     real (kind=long_kind) :: Fstar(2)   ! chage to four sides in two dimension
     real (kind=long_kind) :: Fhat(2)   ! F left at j-1/2  and right at j+1/2 limit
     real (kind=long_kind) :: vhat(2)   ! F left and right limit


  end type elem_t

! ==========================================
! Public Interfaces
! ==========================================
     

  public :: element_coordinates 
  public :: spctral_phy
  public :: phy_spctral
  public :: line_int
  public :: test_phy_spl
contains 




  ! =======================================
  ! element_coordinates:
  !
  ! Initialize 2D rectilinear element 
  ! colocation points
  !
  ! =======================================

  function element_coordinates(id, start,end) result(cart)
    use kinds, only : long_kind
    real (kind=long_kind), intent(in) :: start
    real (kind=long_kind), intent(in) :: end
    integer, intent(in) :: id

    type (elem_t) :: cart

    cart%elem_id = id
    cart%xa = start
    cart%xb = end
    cart%dx = end - start

  end function element_coordinates
subroutine spctral_phy(vphy, nump, vspl, numc)
    
    real(kind=long_kind), dimension(:), intent(in) :: vspl
    real(kind=long_kind), dimension(:), intent(inout) :: vphy
    integer, intent(in) :: nump, numc 
    ! local 
    integer :: i, j 
    
    do  i = 1,nump
	vphy(i) = vspl(1)
	do j = 2, numc
	!write(*,*) "vphy() = ", vphy(i), "leg = ", gll%leg(i,j)
	vphy(i) = vphy(i) + vspl(j)*gll%leg(i,j)
	end do 
    end do 

end subroutine 

subroutine phy_spctral(vphy, nump,  vspl, numc)
    
    real(kind=long_kind), dimension(:), intent(inout) :: vspl
    real(kind=long_kind), dimension(:), intent(in) :: vphy
    integer, intent(in) :: nump, numc 
    ! local 
    integer :: i, j 
    real(kind=long_kind), dimension(nump) :: vtemp
    do  i = 1,numc
	do j = 1, nump
	    vtemp(j) = vphy(j)*gll%leg(j,i)
	end do 
	call line_int(vtemp, vspl(i), nump)
	vspl(i) = vspl(i)*(i-1+0.5)
    end do 

end subroutine 


subroutine line_int(Fvel, Fint, nump) 
     
    real(kind=long_kind), dimension(:),intent(in) :: Fvel 
    real(kind=long_kind),intent(out) :: Fint
    integer, intent(in) :: nump

	! local 
    integer j
    real(kind=long_kind) :: coef
    coef = real((nump-1)*nump)
    Fint = 2/coef*(Fvel(1)+Fvel(nump)) ! to do 
    do  j = 2, nump-1
    !write(*,*) "F=", Fvel(j), "w=", gll%weights(j), "int= ", Fint
        Fint = Fint+ Fvel(j)*gll%weights(j) 
    end do 

end subroutine line_int

subroutine test_phy_spl(nump, numc) 
    integer, intent(in) :: nump, numc 
    
    real(kind=long_kind), dimension(numc) :: vspl
    real(kind=long_kind), dimension(nump) :: vphy
    real(kind=long_kind) :: ftemp

    vphy(:) = 1.0
    call line_int(vphy, ftemp, nump)
    write(*,*) "================== test phy_spl ============"
    write(*,*) "v -> ", vphy
    write(*,*) "INT = ", ftemp
    call phy_spctral(vphy, nump, vspl,numc)
    write(*,*) "vsp -> ", vspl
    call spctral_phy(vphy, nump, vspl,numc)
    write(*,*) "v' -> ", vphy
    write(*,*) "================== test phy_spl  ============"


     
end subroutine test_phy_spl


end module element_mod
