COMPILER=GCC
include ../make.inc
VPATH+=../timer
#FOPT+=-qopenmp-simd
#FOPT=-O3
#FOPT+=$(DEBUG) -check all
#FOPT+=$(DEBUG) -C=all
#FOPT=$(DEBUG)
#FOPT=-ipo
#FOPT+=$(F_IEEE)


all : date.exe read_nml.exe io_error_handling.exe internal_io.exe triads_dynamic.exe body_bm.exe body_structs.exe

triads_dynamic.exe : timer.o dummy.o mod_triads.o triads_dynamic.o
	$(F90) -o $@ $(FOPT) $^

body_bm.exe : timer.o mod_body_bm.o body_bm.o
	$(F90) -o $@ $(FOPT) $^

body_structs.exe : timer.o mod_body_bm.o body_structs.o
	$(F90) -o $@ $(FOPT) $^

%.exe : %.f90
	$(F90) -o $@ $(FOPT) $^

%.o : %.f90
	$(F90) -c -o $@ $(FOPT) $^

clean : 
	rm -rf *.exe *~ *.mod *.o
