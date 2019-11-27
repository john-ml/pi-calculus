#include <stdlib.h>
#include "runtime.c"
gt_ch global11;
struct handler_extra var_extra5;
void var_handler3(gt_ch c)
{
  printf("%d\n", (int)(long)c);
}
void var_listener4(void) {
  for (;;) {
    var_handler3((gt_ch)var_extra5.info);
    gt_t t = current;
    gt_switch(t, current = var_extra5.cause);
  }
}
gt_ch global10;
struct handler_extra var_extra8;
gt_ch var_handler6(void)
{
  int n = 0;
  int c;
  do {
    c = getchar();
  } while (c == '\n' || c == ' ');
  while ('0' <= c && c <= '9') {
    n = 10*n + (c - '0');
    c = getchar();
  }
  return (gt_ch)(long)n;
}
void var_listener7(void) {
  for (;;) {
    *(gt_ch *)var_extra8.info = var_handler6();
    gt_t t = current;
    gt_switch(t, current = var_extra8.cause);
  }
}
gt_ch global9;
struct handler_extra var_extra11;
void var_handler9(gt_ch ch)
{
  printf("here\n");
  gt_ch res = gt_read(ch);
  gt_write(res, (gt_ch)((uint64_t)gt_read(ch) + (uint64_t)gt_read(ch)));
}
void var_listener10(void) {
  for (;;) {
    var_handler9((gt_ch)var_extra11.info);
    gt_t t = current;
    gt_switch(t, current = var_extra11.cause);
  }
}
void var_f0(void) {
  global9 = gt_write_only_chan(var_listener10, &var_extra11, 0x100000);
  global10 = gt_read_only_chan(var_listener7, &var_extra8, 0x100000);
  global11 = gt_write_only_chan(var_listener4, &var_extra5, 0x100000);
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
