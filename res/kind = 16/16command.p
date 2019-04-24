set terminal pdf
set output "wykres16.pdf"
set key box top left
set multi
set logscale x
set logscale y
set key opaque
set key box
set key width 1 height 0.5 font  'Arial, 14' 
set style data lines
set termopt enhanced
set xlabel 'N' font 'Arial,14'
set ylabel 'multiplication time' font 'Arial,14'
set xtics font 'Arial,14'
set ytics font 'Arial,14'
set termoption dashed

plot "result16Naive" title "naive"
replot "result16Better" title "better"
replot "result16Dot" title "dot"
replot "result16MAtmul" title "matmul"

unset multi
