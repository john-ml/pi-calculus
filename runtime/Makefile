all:
	cd tests && gcc -g -I .. -Wall counting.c ../gt_switch.s -o counting
	cd tests && gcc -g -I .. -Wall counting_alloca.c ../gt_switch.s -o counting_alloca
	cd tests && gcc -g -I .. -Wall consumption.c ../gt_switch.s -o consumption
	cd tests && gcc -g -I .. -Wall loops.c ../gt_switch.s -o loops
	cd tests && gcc -g -I .. -Wall write_only.c ../gt_switch.s -o write_only

opt:
	cd tests && gcc -g -I .. -Wall -O2 -D OPT2_FLAG counting.c ../gt_switch.s -o counting
	cd tests && gcc -g -I .. -Wall -O2 -D OPT2_FLAG counting_alloca.c ../gt_switch.s -o counting_alloca
	cd tests && gcc -g -I .. -Wall -O2 -D OPT2_FLAG consumption.c ../gt_switch.s -o consumption
	cd tests && gcc -g -I .. -Wall -O2 -D OPT2_FLAG write_only.c ../gt_switch.s -o write_only
