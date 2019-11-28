#include <stdlib.h>
#include "runtime.c"
void var_f0(void) {
  for (;;) {
    r15 = getchar();
    r15 = putchar(r15);
  }
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 1048576);
  gt_exit(0);
}
