#set terminal post landscape color 'Times-Roman' 20
#set terminal png  xffffff size 800,600 font luximb 16
#set terminal png  xffffff large font VeraBd 12
#set terminal png transparent xffffff size 1000,750 font luximb 18
#set terminal png  xffffff size 1000,750 font luximb 18
set terminal png  size 1000,750 font luximb 18
#set terminal png font VeraBd 12 size 700,600 xffffe0
#set terminal png  size 800,600 font arial 16
set xlabel 'Block size'
set xtics nomirror
set ylabel 'MFlop/s'
set ytics nomirror
set grid ytics mytics
set xrange[1:2000]
set yrange[0.0:6000.0]
set logscale x 10 
set output 'plot_loa_blockdep.png'
set title ''
set key top right
set style line 1 lt 1 lw 3 pt 4 ps 2
set style line 2 lt 2 lw 3 pt 6 ps 2
set style line 3 lt 3 lw 3 pt 3 ps 2
set style line 4 lt 4 lw 3 pt 4 ps 2
set style line 5 lt 6 lw 3 pt 6 ps 2
set style line 6 lt 12 lw 3 pt 1 ps 2
set style line 7 lt 13 lw 3 pt 2 ps 2
set style line 8 lt 14 lw 3 pt 3 ps 2

plot 'Loa_blockdep.res' using 1:2 title 'N ~ 1000' with lines ls 1, \
     'Loa_blockdep.res' using 1:3 title 'N ~ 10000' with lines ls 2, \
     'Loa_blockdep.res' using 1:4 title 'N ~ 100000' with lines ls 3, \
     'Loa_blockdep.res' using 1:5 title 'N ~ 1000000' with lines ls 4

