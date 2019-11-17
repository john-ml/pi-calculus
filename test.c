#include <stdio.h>
#include "runtime.c"

static int n = 0;

void g(void) {
  while (n != 100)
    gt_yield();
  printf("\n");
}

void f(void) {
  for (int i = 0; i < 10; ++i) {
    printf("%d %d ", i, ++n);
    gt_yield();
  }
}

int main(void) {
  gt_init();
  gt_go(g);
  for (int i = 0; i < 10; ++i)
    gt_go(f);
  gt_exit(0);
}
