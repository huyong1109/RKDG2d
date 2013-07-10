INCLDIR  = #-I. -I/apps/intel/impi/4.0.2.003/intel64/include -I/home/hxm/esm-soft/include
CPPFLAGS = #-P -traditional 
CPP      = 
NLIB     = 

FC=  gfortran  $(FFLAGS)
OBJS =  kinds.o control.o physical_constants.o  quadrature_mod.o  dimensions_mod.o   element_mod.o   domain_mod.o  output.o  comm.o \
	init.o  advance.o RKDG.o dg.o

###################################################################################

dg1g: $(OBJS)
	$(FC)  -o dg2d  $(OBJS) $(NLIB)

.SUFFIXES : .F90
.F90.o:
	$(FC) -c  $*.F90  ;
clean:
	rm -f *.o  *.mod  
