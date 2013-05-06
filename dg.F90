! 1d linear/inviscid equation using RKDG method

program dg1d
use kinds, only: long_kind
use control, only: stepnow
use init_mod, only: init
use RKDG, only: SSP_RK
use output_mode
IMPLICIT  NONE 
   integer :: i, tn
   call init()
   call output_vel(1, 'init_vel1.bin')
   call output_vsp(1, 'init_spl.bin')
   call SSP_RK()
   call stepnow(tn)
   call output_vel(tn,"end_vel1.bin")
   
   
END program dg1d


    




