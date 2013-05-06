module output_mode

use kinds, only: long_kind
use domain_mod, only: ele, nelem_local
use quadrature_mod, only: quadrature_t 

implicit none 
private 

public :: output_vel, output_vsp
contains 
subroutine output_vel(T_lel, ff)
   integer, intent(in) :: T_lel 
   character(*) ::  ff
   ! local 
   integer i
   integer :: vsize
   !character(len=*) ::  ff
   vsize = size(ele(i)%v(:,T_lel))
   write(*,*) vsize
   write(*,*) 'output velocity '
   if (trim(ff)  == 'stdout') then
       do i = 1,nelem_local
      write(*,*) ele(i)%v(:,T_lel)
     end do 
   else 
     open(unit=10, file=ff, form='unformatted', status = 'replace')
     do i = 1,nelem_local
	write(10) ele(i)%v(:,T_lel)
     end do 
     close(10)
   end if 
end subroutine output_vel

subroutine output_vsp(T_lel, ff)
   integer, intent(in) :: T_lel 
   character(*) ::  ff
   !local 
   integer :: vsize
   integer i
   
   !character(len=*) ::  ff
   vsize = size(ele(i)%vsp(:,T_lel))
   write(*,*) 'output spectral '
   if (trim(ff)  == 'stdout') then
     do i = 1, nelem_local
      write(*,*) ele(i)%vsp(:,T_lel)
     end do 
   else 
     open(unit=10, file=ff, form='unformatted', status = 'replace')
     do i = 1, nelem_local
      write(10) ele(i)%vsp(:,T_lel)
     end do 
     close(10)
   end if 
end subroutine output_vsp

end module output_mode
