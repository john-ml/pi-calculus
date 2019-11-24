#include <stdlib.h>
#include <stdio.h>
#include "runtime.c"
void var_f0(void) {
  rbx = gt_chan();
  for (;;) {
    gt_write(r12, rbx);
  }
}
int main(void) {
  gt_init();
  r12 = gt_chan();
  gt_t var_t1 = gt_go(var_f0, 0x100000);
  var_t1->r12 = r12;
  for (;;) {
    r15 = gt_read(r12);
    printf("%lu", (gt_ch)r15 - channels);
  }
  gt_exit(0);
}

