COMPILER=Intel
VPATH+=../timer 
include ../make.inc
#FOPT:=$(DEBUG)


all : test_rational.exe test_rational_reduction.exe logging.exe heat_dynamic.exe heat_ptr.exe transpose_rank3.exe heat_io.exe
special : plot_quadratic.exe

test_rational.exe : rational.f90 test_rational.f90
	$(F90) -o $@  $^
test_rational_reduction.exe : rational.f90 rational_reduction.f90 test_rational_reduction.f90
	$(F90) -o $@  $^
heat_dynamic.exe : mod_heat_dynamic.f90 ftn_getopt.o heat_dynamic.f90 timer.o
	$(F90) $(FOPT) -o $@ $^
heat_ptr.exe : mod_heat_ptr.f90 ftn_getopt.o heat_ptr.f90 timer.o
	$(F90) $(FOPT) -o $@ $^
heat_io.exe : mod_heat_io.f90 ftn_getopt.o heat_io.f90 timer.o
	$(F90) $(FOPT) -o $@ $^
plot_quadratic.exe : mod_solver.f90 plot_quadratic.f90
	$(F90) $(FOPT) $(DEBUG) $(PLPLOT_INC) -o $@ $^ $(PLPLOT_F95_SHLIB)
transpose_rank3.exe : transpose_rank3.f90
	$(F90) $(FFLAGS) -o $@  $^


%.exe : %.f90
	$(F90) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf *~ *.o *.exe  *.mod log*.txt *.ckp *.cfg *.dat
