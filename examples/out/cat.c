#include <stdlib.h>
#include "runtime.c"
gt_ch global5;
struct handler_extra var_extra5;
void var_handler3(gt_ch c)
{ putchar((int)(long)c); }
void var_listener4(void) {
  for (;;) {
    var_handler3((gt_ch)var_extra5.info);
    gt_t t = current;
    gt_switch(t, current = var_extra5.cause);
  }
}
gt_ch global4;
struct handler_extra var_extra8;
gt_ch var_handler6(void)
{ return (gt_ch)(long)getchar(); }
void var_listener7(void) {
  for (;;) {
    *(gt_ch *)var_extra8.info = var_handler6();
    gt_t t = current;
    gt_switch(t, current = var_extra8.cause);
  }
}
void var_f0(void) {
  global4 = gt_read_only_chan(var_listener7, &var_extra8, 0x100000);
  global5 = gt_write_only_chan(var_listener4, &var_extra5, 0x100000);
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
