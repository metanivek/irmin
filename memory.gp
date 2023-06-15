DATA = ARG1
LABEL = ARG2

set terminal pngcairo truecolor size 1600,900
set output './'.DATA.'.stats.png'

set multiplot layout 1,1
set lmargin 8

# set title 'Disk space used (5/6 rolling cycles)'
# set key inside bottom

# set format y "%3gGb"
# set format x "%3gm"

# set yrange [0:]
# set xrange [0:]
# # set xrange [44:49]
# set xtics 10  nomirror
# set mxtics 10
# set mytics 10

# plot DATA using (($1) / 60):($2 / (1024 * 1024)) with lines lc 1 title LABEL

# unset label

set title 'Memory used'
set format y "%4gMb"

stats DATA using ($3 / 1024) prefix "A" nooutput
max_a(x) = A_max
plot DATA using (($1) / 60):($3 / 1024) with lines lc 1 notitle, \
     max_a(x) with lines lc 1 lt 1 dt 3 title sprintf("%s max = %g", LABEL, A_max)


unset multiplot
