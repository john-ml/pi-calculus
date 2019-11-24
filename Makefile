all:
	cd src && gcc -g -Wall counting.c gt_switch.s -o ../bin/counting
	cd src && gcc -g -Wall consumption.c gt_switch.s -o ../bin/consumption
	cd src && gcc -g -Wall loops.c gt_switch.s -o ../bin/loops

opt:
	cd src && gcc -g -Wall -O2 -D OPT2_FLAG counting.c gt_switch.s -o ../bin/counting
	cd src && gcc -g -Wall -O2 -D OPT2_FLAG consumption.c gt_switch.s -o ../bin/consumption
