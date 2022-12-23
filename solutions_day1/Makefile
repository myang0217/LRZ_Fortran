COMPILER=GCC
include ../make.inc
# you might need to change the value below to point at your BLAS installation
BLAS_SHLIB=-L/usr/lib/x86_64-linux-gnu/ -lblas
# uncomment the following if Intel MKL is installed and gfortran is used
#BLAS_SHLIB=-L${MKLROOT}/lib/intel64 -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl
# uncomment the following if Intel MKL is installed and ifort is used
#BLAS_SHLIB=-L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl



all : pi_approx.exe calculate_dot_product.exe solve_my_quadratic.exe  \
      sum_of_squares.exe triangle.exe sieve.exe test_quadratic.exe \
      test_gcd.exe sort_string.exe

pi_approx.exe : pi_approx.f90
	$(F90) $(FOPT) -o $@ $^
calculate_dot_product.exe : blas77.f90 calculate_dot_product.f90
	$(F90) -o $@ $^ $(BLAS_SHLIB)
test_quadratic.exe : mod_solver.f90 test_quadratic.f90
	$(F90) -o $@ $^
test_gcd.exe : mod_gcd.f90 test_gcd.f90
	$(F90) -o $@ $^

%.exe : %.f90
	$(F90) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf *~ *.exe  *.mod
