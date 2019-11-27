#include <stdlib.h>
#include "runtime.c"
gt_ch global11;
void var_r3(gt_ch c) {
  printf("%d\n", (int)(long)c);
}
void var_h4(void) {
  for (;;) {
    var_r3(gt_read(global11));
    gt_yield();
  }
}
gt_ch global10;
gt_ch var_w5(void) {
  int n = 0;
    int c;
    do {c = getchar();
    }while (c == '\n' || c == ' ');
    while ('0' <= c && c <= '9') {n = 10*n + (c - '0');
      c = getchar();
    }return (gt_ch)(long)n;
}
void var_h6(void) {
  for (;;) {
    gt_write(global10, var_w5());
    gt_yield();
  }
}
gt_ch global9;
void var_r7(gt_ch ch) {
  gt_ch res = gt_read(ch);
    gt_write(res, (gt_ch)((uint64_t)gt_read(ch) + (uint64_t)gt_read(ch)));
}
void var_h8(void) {
  for (;;) {
    var_r7(gt_read(global9));
    gt_yield();
  }
}
void var_f0(void) {
  global9 = gt_chan();
  gt_go(var_h8, 0x10000);
  global10 = gt_chan();
  gt_go(var_h6, 0x10000);
  global11 = gt_chan();
  gt_go(var_h4, 0x10000);
  rbx = gt_read(global10);
  r15 = gt_read(global10);
  r12 = gt_chan();
  r13 = gt_chan();
  gt_write(global9, r12);
  gt_write(r12, r13);
  gt_write(r12, rbx);
  gt_write(r12, r15);
  r15 = gt_read(r13);
  gt_write(global11, r15);
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 512);
  gt_exit(0);
}
