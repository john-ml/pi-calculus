#include <stdlib.h>
#include "runtime.c"
gt_val read_int(void) {
  int c;
  return scanf("%d", &c) == 1 ? (gt_val)(long)c : NULL;
}

void write_int(gt_val c) { printf("%d\n", (int)(long)c); }

gt_val add(gt_val x, gt_val y) { return (gt_val)((long)x + (long)y); }

void var_f0(void) {
  for (;;) {
    write_int(add(read_int(),read_int()));
  }
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 1048576);
  gt_exit(0);
}
