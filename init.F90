module init_mod 
use kinds, only: long_kind, longdouble_kind
use control, only: init_control
use dimensions_mod, only :  a, b, nelem, np, nc, init_dimension, gll
use element_mod
use domain_mod
    implicit none 
    private 
    

    public :: init, init_val
    contains
subroutine init
    write(*,*) "Start initiating "
    call init_control 
    call init_dimension
    !call test_phy_spl(np,nc)

    call init_domain
    call init_val

    write(*,*) "End initiating "

end subroutine init 
subroutine init_val()
    integer :: n, i
    real(kind=long_kind) :: length, xa, xb
    real(kind=long_kind) :: vtemp(np)
    !real(kind=longdouble_kind),dimension(:), pointer :: points
111 FORMAT( 5X, E11.5\)
    write(*,*) "Set inital value "
    !points => gll%points    
    !write(*,*) "points = ", SIZE(gll%points)
     
    length = 1.0/real(nelem)*(b-a)
    !write(*,*) real(nelem), a , b 
    write(*,*) "======================================= "
    write(*,*) "	inital value"
    do n = domain%start, domain%end
	xa = a  + real(n-1)*length
	xb = xa +length
	ele(n) = element_coordinates(n,xa, xb)
	write(*,"(I3\)") n
	!do i = 1, np
	vtemp(:) = xa+(1.0 +gll%points(1:np))/2.0*(xb-xa)
	!write(*,111) vtemp(:)
	vtemp(:) = init_f(vtemp, np)
	write(*,111) vtemp(:)
	!end do 
	write(*,*) " "
	ele(n)%v(:,1) = vtemp
	! for test
	!write(*,*) "================== v -> vsp ================="
	!write(*,*) ele(n)%v(:,1)
	call phy_spctral( ele(n)%v(:,1), np, ele(n)%vsp(:,1), nc)
	!write(*,*) ele(n)%vsp(:,1)
	!call spctral_phy(ele(n)%v(:,1), np, ele(n)%vsp(:,1), nc)
	!write(*,*) ele(n)%v(:,1)
    

    end do 
    write(*,*) "======================================= "
end subroutine init_val

function  init_f(vec, nump)
use control, only: finit 
    integer, intent(in) :: nump
    real(kind=long_kind), intent(in) ::vec(nump) 
    real(kind=long_kind) :: init_f(nump)
    !local
    integer :: i 
    select case(finit)
    case (1)
	write(*,*) "use linear"
	do i = 1, nump
	init_f(i) = init_guasshill(vec(i))
	end do 
    case (2) 
	write(*,*) "use discon"
	do i = 1, nump
	init_f(i) = init_discontinous(vec(i))
	end do 
    case(3)
	write(*,*) "use sin"
	do i = 1, nump
	init_f(i) = init_sin(vec(i))
	end do 
    end select
    return

end function init_f

	


function init_guasshill(x) result(fx)
    real(kind=long_kind), intent(in) :: x 
    real(kind=long_kind)  :: fx 

    fx = exp(-8.0*x**2)

end function init_guasshill

function init_discontinous(x) result(fx)
    real(kind=long_kind), intent(in) :: x 
    real(kind=long_kind)  :: fx 
    if (x < 0.2*a .OR. x >0.2*b) then
	fx = 0.0 
    else if ( 0.2*a <x  .AND. x<0.2*b) then
	fx = 1.0 
    else
	write(*,*) "Variable should in (", a,",",b,")"
    endif 
end function init_discontinous

function init_sin(x) result(fx)
    use physical_constants, only : DD_PI
    real(kind=long_kind), intent(in) :: x 
    real(kind=long_kind)  :: fx 

    fx = 0.5 + sin(DD_PI*x)
end function init_sin



end module init_mod


