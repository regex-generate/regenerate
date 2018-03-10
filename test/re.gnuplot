# Gnuplot script file for plotting data in file "force.dat"
# This file is called   force.p
set   autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
#set title "Force Deflection Data for a Beam and a Column"
set ylabel "Count"
set xlabel "Time (s)"
#set logscale y

set terminal png size 1200,900 enhanced
set output 're_gen.png'

plot "re_stream_0.csv" using 2:1 title 'Re 0 Stream' with linespoints , \
     "re_stream_1.csv" using 2:1 title 'Re 1 Stream' with linespoints , \
     "re_stream_2.csv" using 2:1 title 'Re 2 Stream' with linespoints , \
     "re_stream_3.csv" using 2:1 title 'Re 3 Stream' with linespoints, \
     "re_trie_0.csv" using 2:1 title 'Re 0 Trie' with linespoints , \
     "re_trie_1.csv" using 2:1 title 'Re 1 Trie' with linespoints , \
     "re_trie_2.csv" using 2:1 title 'Re 2 Trie' with linespoints , \
     "re_trie_3.csv" using 2:1 title 'Re 3 Trie' with linespoints, \
     "re_set_0.csv" using 2:1 title 'Re 0 Set' with linespoints , \
     "re_set_1.csv" using 2:1 title 'Re 1 Set' with linespoints , \
     "re_set_2.csv" using 2:1 title 'Re 2 Set' with linespoints , \
     "re_set_3.csv" using 2:1 title 'Re 3 Set' with linespoints