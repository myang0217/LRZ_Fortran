COMPILER=GCC
include ../make.inc
VPATH+=../timer
#FOPT+=-qopenmp-simd
#FOPT=-O3
#FOPT+=$(DEBUG) -check all

all : triads.exe implied_do.exe derived_type.exe global_data.exe arrays.exe array_intrinsics.exe specifications.exe execute_cmdline.exe


triads.exe : timer.o dummy.o mod_triads.o triads.o 
	$(F90) -o $@ $(FOPT) $^

%.exe : %.f90
	$(F90)  -o $@ $(FOPT) $^
%.o : %.f90
	$(F90) -c -o $@ $(FOPT) $^

clean : 
	rm -rf *.exe *~ *.mod *.o
