#set terminal post landscape color 'Times-Roman' 20
#set terminal png  xffffff size 800,600 font luximb 16
#set terminal png  xffffff large font VeraBd 12
#set terminal png transparent xffffff size 1000,750 font luximb 18
#set terminal png  xffffff size 1000,750 font luximb 18
set terminal png  size 1000,750 font luximb 18
#set terminal png font VeraBd 12 size 700,600 xffffe0
#set terminal png  size 800,600 font arial 16
set xlabel 'Vector length'
set xtics nomirror
set ylabel 'MFlop/s'
set ytics nomirror
set grid ytics mytics
set xrange[60:20000000]
set yrange[0.0:6000.0]
set logscale x 10 
set output 'plot_loa.png'
set title 'LoA on Sandy Bridge 2.3 GHz with AVX / ifort 16.0' 
set key top right
set style line 1 lt 1 lw 3 pt 4 ps 2
set style line 2 lt 2 lw 3 pt 6 ps 2
set style line 3 lt 3 lw 3 pt 3 ps 2
set style line 4 lt 4 lw 3 pt 4 ps 2
set style line 5 lt 6 lw 3 pt 6 ps 2
set style line 6 lt 12 lw 3 pt 1 ps 2
set style line 7 lt 13 lw 3 pt 2 ps 2
set style line 8 lt 14 lw 3 pt 3 ps 2

plot 'SandyBridge_2.3_AVX_loa_bsize1.res' using 1:3 title 'idim 1' with lines ls 1, \
     'SandyBridge_2.3_AVX_loa_bsize2.res' using 1:3 title 'idim 2' with lines ls 2, \
     'SandyBridge_2.3_AVX_loa_bsize8.res' using 1:3 title 'idim 8' with lines ls 3, \
     'SandyBridge_2.3_AVX_loa_bsize32.res' using 1:3 title 'idim 32' with lines ls 4, \
     'SandyBridge_2.3_AVX_loa_bsize128.res' using 1:3 title 'idim 128' with lines ls 5, \
     'SandyBridge_2.3_AVX_dp.res' using 1:3 title 'reference triad' with lines ls 6
