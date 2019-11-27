#include <stdlib.h>
#include "runtime.c"
gt_ch global5;
void var_r3(gt_ch c) {
  fputc((int)(long)c, stdout); 
}
void var_h4(void) {
  for (;;) {
    var_r3(gt_read(global5));
  }
}
gt_ch global4;
gt_ch var_w5(void) {
  return (gt_ch)(long)fgetc(stdin); 
}
void var_h6(void) {
  for (;;) {
    gt_write(global4, var_w5());
  }
}
void var_f0(void) {
  global4 = gt_chan();
  gt_go(var_h6, 0x10000);
  global5 = gt_chan();
  gt_go(var_h4, 0x10000);
  for (;;) {
    r15 = gt_read(global4);
    gt_write(global5, r15);
  }
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 512);
  gt_exit(0);
}
