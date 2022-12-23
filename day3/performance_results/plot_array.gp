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
set y2label '%'
set ytics nomirror
set y2tics 
set grid ytics mytics
set xrange[60:18000000]
set x2range[60:18000000]
set yrange[0.0:11000.0]
set y2range[0.0:250.0]
set logscale x 10 
set logscale x2 10 
set output 'plot_arrayperf.png'
set title 'Skylake 2.3 GHz with AVX512 / ifort 19.0' 
set key top right
set style line 1 lt 1 lw 3 pt 4 ps 2
set style line 2 lt 2 lw 3 pt 6 ps 2
set style line 3 lt 3 lw 3 pt 3 ps 2
set style line 4 lt 4 lw 3 pt 4 ps 2
set style line 5 lt 6 lw 3 pt 6 ps 2
set style line 6 lt 12 lw 3 pt 1 ps 2
set style line 7 lt 13 lw 3 pt 2 ps 2

plot 'Skylake_Array_Ifort19.0.dat' using 1:2 title 'DP baseline' with lines ls 1,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$3/$2) title 'array syntax %' with lines ls 2 axis x2y2,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$4/$2) title 'assumed shape %' with lines ls 3 axis x2y2,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$5/$2) title 'contig assumed shape %' with lines ls 4 axis x2y2

set output 'plot_pointerperf.png'
plot 'Skylake_Array_Ifort19.0.dat' using 1:2 title 'DP baseline' with lines ls 1,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$6/$2) title 'pointer intent(in) %' with lines ls 2 axis x2y2,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$7/$2) title 'pointer contiguous %' with lines ls 3 axis x2y2,\
     'Skylake_Array_Ifort19.0.dat' using 1:(100*$8/$2) title 'pointer intent(inout) %' with lines ls 4 axis x2y2
