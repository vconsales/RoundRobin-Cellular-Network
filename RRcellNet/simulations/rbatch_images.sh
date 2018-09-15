#!/bin/bash

r analyze_csv.r <<EOF
mode png
all unif
close
all unifbest
close
all binom
close
all binombest
close
exit
EOF
