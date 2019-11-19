all:
	gcc -g -Wall counting.c gt_switch.s -o counting
	gcc -g -Wall consumption.c gt_switch.s -o consumption

opt:
<<<<<<< HEAD
	cd src && gcc -g -Wall -O2 -D OPT2_FLAG counting.c gt_switch.s -o ../bin/counting
	cd src && gcc -g -Wall -O2 -D OPT2_FLAG consumption.c gt_switch.s -o ../bin/consumption
=======
	gcc -g -Wall -O2 counting.c gt_switch.s -o counting
	gcc -g -Wall -O2 consumption.c gt_switch.s -o consumption
>>>>>>> parent of 1a647ee... Cleanup
