#set terminal post landscape color 'Times-Roman' 20
#set terminal png  xffffff size 800,600 font luximb 16
#set terminal png  xffffff large font VeraBd 12
#set terminal png transparent xffffff size 1000,750 font luximb 18
#set terminal png  xffffff size 1000,750 font luximb 18
#set terminal png  size 1000,750 font luximb 18
set terminal png  size 1000,750 font Vera 16 
#set terminal png font VeraBd 12 size 700,600 xffffe0
#set terminal png  size 800,600 font arial 16
set xlabel 'Vector length'
set xtics nomirror
set ylabel 'MFlop/s'
set ytics nomirror
set grid ytics mytics
set xrange[60:18000000]
set yrange[0.0:11000.0]
set logscale x 10 
set key top right
set style line 1 lt 1 lw 3 pt 4 ps 2
set style line 2 lt 2 lw 3 pt 6 ps 2
set style line 3 lt 3 lw 3 pt 3 ps 2
set style line 4 lt 4 lw 3 pt 4 ps 2
set style line 5 lt 6 lw 3 pt 6 ps 2
set style line 6 lt 12 lw 3 pt 1 ps 2
set style line 7 lt 13 lw 3 pt 2 ps 2

set output 'plot_compiler_contig_assumed_shape.png'
set title 'Skylake 2.3 GHz Contiguous Assumed Shape' 
plot 'Skylake_Array_Ifort19.0.dat' using 1:5 title 'Ifort 19.0' with lines ls 1,\
     'Skylake_Array_Gfortran9.2.dat' using 1:5 title 'Gfortran 9.2' with lines ls 2,\
     'Skylake_Array_NAG7.0.dat' using 1:5 title 'NAGfor 7.0' with lines ls 3, \
     'Skylake_Array_PGI19.10.dat' using 1:5 title 'PGI 19.10' with lines ls 4

set output 'plot_compiler_pointer_in.png'
set title 'Skylake 2.3 GHz POINTER intent in' 
plot 'Skylake_Array_Ifort19.0.dat' using 1:6 title 'Ifort 19.0' with lines ls 1,\
     'Skylake_Array_Gfortran9.2.dat' using 1:6 title 'Gfortran 9.2' with lines ls 2,\
     'Skylake_Array_NAG7.0.dat' using 1:6 title 'NAGfor 7.0' with lines ls 3, \
     'Skylake_Array_PGI19.10.dat' using 1:6 title 'PGI 19.10' with lines ls 4

set output 'plot_compiler_pointer_contiguous.png'
set title 'Skylake 2.3 GHz CONTIGUOUS POINTER' 
plot 'Skylake_Array_Ifort19.0.dat' using 1:7 title 'Ifort 19.0' with lines ls 1,\
     'Skylake_Array_Gfortran9.2.dat' using 1:7 title 'Gfortran 9.2' with lines ls 2,\
     'Skylake_Array_NAG7.0.dat' using 1:7 title 'NAGfor 7.0' with lines ls 3, \
     'Skylake_Array_PGI19.10.dat' using 1:7 title 'PGI 19.10' with lines ls 4

set output 'plot_compiler_pointer_inout.png'
set title 'Skylake 2.3 GHz POINTER intent inout' 
set yrange[0.0:3000.0]
plot 'Skylake_Array_Ifort19.0.dat' using 1:8 title 'Ifort 19.0' with lines ls 1,\
     'Skylake_Array_Gfortran9.2.dat' using 1:8 title 'Gfortran 9.2' with lines ls 2,\
     'Skylake_Array_NAG7.0.dat' using 1:8 title 'NAGfor 7.0' with lines ls 3, \
     'Skylake_Array_PGI19.10.dat' using 1:8 title 'PGI 19.10' with lines ls 4

