#include <runtime.c>
#include <stdio.h>

void f(void) {
  int c;
  scanf("%d", &c);
  printf("Read in %d\n", c);
  asm ("jmp gt_stop\t\n");
}

int main(void) {
  gt_init();
  gt_go(f, 0x100000);
  gt_exit(0);
}
