#include <stdlib.h>
#include "runtime.c"
void var_f3(void) {
  gt_ch *rsp = (gt_ch *)gt_self()->rsp + 1;
  gt_ch spill0 = rsp[0];
  gt_ch spill1 = rsp[1];
  gt_write(spill0, spill1);
  gt_write(spill0, rbx);
  gt_write(spill0, r15);
  gt_write(spill0, r14);
  gt_write(spill0, r12);
  gt_write(spill0, r13);
  asm ("jmp gt_stop\t\n");
}
void var_f0(void) {
  gt_ch spill0 = gt_chan();
  gt_ch spill1 = gt_chan();
  rbx = gt_chan();
  r15 = gt_chan();
  r14 = gt_chan();
  r12 = gt_chan();
  r13 = gt_chan();
  gt_t var_t4 = gt_go_alloca(var_f3, 16, 272);
  var_t4->rbx = rbx;
  var_t4->r15 = r15;
  var_t4->r14 = r14;
  var_t4->r12 = r12;
  var_t4->r13 = r13;
  gt_ch *var_rsp5 = ((gt_ch *)var_t4->rsp) + 1;
  var_rsp5[0] = spill0;
  var_rsp5[1] = spill1;
  gt_write(spill0, rbx);
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 256);
  gt_exit(0);
}
