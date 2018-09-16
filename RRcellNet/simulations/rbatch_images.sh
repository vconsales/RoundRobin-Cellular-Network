#!/bin/bash

# batch generate all the images
r analyze_csv.r <<EOF
mode png
all binom
close
all binombest
close
ecdf unif unifbest 2.1 0
close
ecdf unif unifbest 3.1 0
close
lorallrt unifbest
close
lorallth unifbest
close
all val1
close
all val2
close
rb val2 val2 0 1
close
thantenna 1
close
all unif
close
all unifbest
close
exit
EOF

# move all images to doc images folder
mv *.png ../../docs/maindoc/images/