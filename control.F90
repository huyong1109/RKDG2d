module control
use kinds, only: int_kind, long_kind
implicit none
private
     ! time control 
     integer(kind=int_kind):: tnow = 1

     real(kind=long_kind), public :: dt
     integer(kind=int_kind), public :: nstep
     ! function control 
     integer, public :: finit, fexp 


     public :: stepnow, stepnext, nextstep
     public :: init_control
     public :: Ffun
contains
subroutine init_control
     integer :: nml_error
     namelist /control_nml/ dt, nstep
     namelist /functon_nml/ finit, fexp


     ! set default time value
     dt = 0.01
     nstep = 1

     open(10, file='param_in', status='old', iostat=nml_error)
     write(*,*) nml_error
     if(nml_error > 0) then
	 write(*,*) "FILE param_in open error in control initiation!!!"
	 write(*,*) "Use default time control "
     endif 
     !do while(nml_error == 0)
     read(10, nml=control_nml, iostat=nml_error)
     !end do 
     if(nml_error >0) then 
	 write(*,*) "namelist read error!!!"
	 stop
     endif 
     write(*,*) "=============================="
     write(*,*) "    time control set "
     write(*,nml=control_nml)
     write(*,*) "=============================="

     ! set default function choice
     finit = 1
     fexp =  1

     
     nml_error = 0 
     !do while(nml_error == 0)
     read(10, nml=functon_nml, iostat=nml_error)
     !end do 
     if(nml_error >0) then 
	 write(*,*) "namelist read error!!!"
	 stop
     endif 


     write(*,*) "=============================="
     write(*,*) "    function  "
     write(*,nml=functon_nml)
     write(*,*) "=============================="

     close(10)

end subroutine init_control
subroutine stepnow(tn)
    integer, intent(inout) :: tn 
    tn = tnow
    
end subroutine stepnow
subroutine stepnext(tn)
    integer, intent(inout) :: tn 
    tn = 1+mod(tnow,2)

end subroutine stepnext
subroutine nextstep
    
    tnow = 1+mod(tnow,2)

end subroutine nextstep

subroutine Ffun(vel,Ff, nump)
use kinds, only: long_kind
    integer, intent(in) :: nump
    real(kind=long_kind), dimension(nump),intent(in) :: vel
    real(kind=long_kind), dimension(nump),intent(inout) :: Ff
    
    integer :: i 
    !write(*,*) "vel in Ffun" , vel(:)
    select case(fexp)
    case (1)
    do i  = 1, nump
	Ff(i) = fun_linear(vel(i))
		
    end do 
    case (2)
    do i  = 1, nump
	Ff(i) = fun_visid(vel(i))
    end do 
    case default
    write(*,*) "====================================="
    write(*,*) "Ffun should be one of the following: "
    write(*,*) "    1		 linear " 
    write(*,*) "    2		 visid  " 
    write(*,*) "====================================="
    end select
   ! write(*,*)	"vel = ",  vel 
   ! write(*,*)	"Ff  = ",  Ff
end subroutine Ffun

function fun_linear(x)
use kinds, only: long_kind
    real(kind=long_kind) :: x
    real(kind=long_kind) :: fun_linear

    fun_linear = x

end function  
function fun_visid(x)
use kinds, only: long_kind
    real(kind=long_kind) :: x
    real(kind=long_kind) :: fun_visid

    fun_visid = 0.5*x*x

end function 
     
end module control
