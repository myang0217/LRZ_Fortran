COMPILER=GCC
include ../make.inc


all : character_entities.exe ascii_characters.exe integer_entities.exe \
      real_entities.exe loops.exe model_intrinsics.exe simple_io.exe \
      interoperable_interface.exe conditional.exe functions.exe arrays.exe \



%.exe : %.f90
	$(F90) -o $@  $^

%.o : %.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf *~ *.exe  *.mod
