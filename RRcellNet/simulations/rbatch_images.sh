#!/bin/bash

# batch generate all the images
r analyze_csv.r <<EOF
mode png
all binombest
close
all binom
close
all nofram
close
allrbbars binom binombest 4.1
close
allrbbars binom binombest 9.1
close
allrbbars unif unifbest 1.1
close
all unifbest
close
all unif
close
all val1
close
all val2
close
ecdf unif unifbest 2.1 0
close
ecdf unif unifbest 3.1 0
close
fillrb nofram
close
lorallrb binombest
close
lorallrt unifbest
close
lorallth binom
close
lorallth binombest
close
lorallth unifbest
close
rb val2 val2 0 1
close
thantenna 1
close
thantenna 2
close
thantennamax 1
close
th binom binombest 5 5
close
exit
EOF

# move all images to doc images folder
mv *.png ../../docs/maindoc/images/