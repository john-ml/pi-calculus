#include <stdio.h>
#include "runtime.c"

void f(void) {
  static int n = 0;
  for (int i = 0; i < 10; ++i) {
    printf("%d %d\n", i, ++n);
    gt_yield();
  }
}

int main(void) {
  gt_init();
  gt_go(f);
  gt_go(f);
  gt_exit(0);
}
