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
set yrange[0.0:9500.0]
set logscale x 10 
set output 'plot_types.png'
set title 'Sandy Bridge 2.3 GHz with AVX / ifort 16.0' 
set key top right
set style line 1 lt 1 lw 3 pt 4 ps 2
set style line 2 lt 2 lw 3 pt 6 ps 2
set style line 3 lt 3 lw 3 pt 3 ps 2
set style line 4 lt 4 lw 3 pt 4 ps 2
set style line 5 lt 6 lw 3 pt 6 ps 2
set style line 6 lt 12 lw 3 pt 1 ps 2

plot 'SandyBridge_2.3_AVX_dp.res' using 1:3 title 'double prec' with lines ls 1, \
     'SandyBridge_2.3_AVX_sp.res' using 1:3 title 'single prec' with lines ls 2, \
     'SandyBridge_2.3_AVX_qp.res' using 1:3 title '16 Byte real' with lines ls 5, \
     'SandyBridge_2.3_AVX_di.res' using 1:3 title 'default int' with lines ls 3, \
     'SandyBridge_2.3_AVX_li.res' using 1:3 title 'large int' with lines ls 4, \
     'SandyBridge_2.3_AVX_dc.res' using 1:3 title 'double cmplx' with lines ls 6
