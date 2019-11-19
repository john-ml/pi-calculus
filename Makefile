all:
	cd src && gcc -g -Wall counting.c gt_switch.s -o ../bin/counting
	cd src && gcc -g -Wall consumption.c gt_switch.s -o ../bin/consumption

opt:
	cd src && gcc -g -Wall -O2 counting.c gt_switch.s -o ../bin/counting
	cd src && gcc -g -Wall -O2 consumption.c gt_switch.s -o ../bin/consumption
