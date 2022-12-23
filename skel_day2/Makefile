COMPILER=GCC
VPATH+=../timer
include ../make.inc
# you might need to change the value below to point at your BLAS installation
BLAS_SHLIB=-L/usr/lib/x86_64-linux-gnu/ -lblas
# uncomment the following if Intel MKL is installed and gfortran is used
#BLAS_SHLIB=-L${MKLROOT}/lib/intel64 -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl
# uncomment the following if Intel MKL is installed and ifort is used
#BLAS_SHLIB=-L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl

# Intel only:
# FOPT+=-parallel
#FOPT+=$(DEBUG)


all : shapes.exe heat_static.exe heat_par.exe calculate_dot_product_const.exe scores.exe sieve_arrays.exe example_getopt.exe
special : plot_rand_hist.exe

body.exe :: mod_body.f90 mod_charged_body.f90 prog_body.f90
	$(F90) $(FOPT) -o $@ $^
shapes.exe : mod_shapes.f90
	$(F90) $(FOPT) -o $@ $^
heat_static.exe : mod_heat_static.f90 heat_static.f90 timer.o
	$(F90) $(FOPT) -o $@ $^
heat_par.exe : mod_heat_par.f90 heat_par.f90 timer.o
	$(F90) $(FPAR) $(FOPT) -o $@ $^
scores.exe : scores.f90
	$(F90) $(FOPT) -o $@ $^
plot_rand_hist.exe : mod_rand.f90 plot_rand_hist.f90
	$(F90) $(FGSL_INC) $(PLPLOT_INC) -o $@ $^ $(FGSL_SHLIB) $(GSL_SHLIB) $(GSL_BLAS_SHLIB) $(PLPLOT_F95_SHLIB)
calculate_dot_product_const.exe : blas77.f90 calculate_dot_product_const.f90
	$(F90) -o $@ $^ $(BLAS_SHLIB)
sieve_arrays.exe : ftn_getopt.f90 sieve_arrays.f90
	$(F90) $(FPAR) $(FOPT) -o $@ $^
example_getopt.exe : ftn_getopt.f90 example_getopt.f90
	$(F90) $(FPAR) $(FOPT) -o $@ $^


%.exe : %.f90
	$(F90) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf *~ *.o *.exe  *.mod
