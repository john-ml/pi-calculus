#include <stdlib.h>
#include "runtime.c"
void var_f3(void) {
  for (;;) {
    r15 = gt_read(r12);
  }
  asm ("jmp gt_stop\t\n");
}
void var_f0(void) {
  rbx = gt_chan();
  r12 = gt_chan();
  gt_t var_t4 = gt_go(var_f3, 512);
  var_t4->r12 = r12;
  for (;;) {
    gt_write(rbx, r12);
  }
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 512);
  gt_exit(0);
}
