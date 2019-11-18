all:
	gcc -g -Wall counting.c gt_switch.s -o counting
	gcc -g -Wall consumption.c gt_switch.s -o consumption

opt:
	gcc -g -Wall -O2 counting.c gt_switch.s -o counting
	gcc -g -Wall -O2 consumption.c gt_switch.s -o consumption
