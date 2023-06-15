DATA = ARG1
LABEL = ARG2

set terminal pngcairo truecolor size 1600,900
set output './'.DATA.'.stats.png'

set multiplot layout 1,1
set lmargin 8

set title 'Memory used'
set format y "%4gMb"

stats DATA using ($3 / 1024) prefix "A" nooutput
max_a(x) = A_max
stats DATA using ($4 / 1024) prefix "B" nooutput
max_b(x) = B_max
plot DATA using (($1) / 60):($3 / 1024) with lines lc 1 notitle, \
     max_a(x) with lines lc 1 lt 1 dt 3 title sprintf("%s max = %g", LABEL, A_max), \
     max_b(x) with lines lc 1 lt 1 dt 3 title sprintf("%s max = %g", LABEL.'_C', B_max)

unset multiplot
