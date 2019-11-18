#include <stdio.h>
#include "runtime.c"

static int n = 0;

void g(void) {
  while (n != 100)
    gt_yield();
  puts("\n");
}

void f(void) {
  for (int i = 0; i < 10; ++i) {
    printf("%d %d ", i, ++n);
    gt_yield();
  }
}

int main(void) {
  gt_init();
  // If unoptimized
  {
    gt_go(g, 64); // No printf ==> don't really need any space
    for (int i = 0; i < 10; ++i)
      gt_go(f, 4096); // Need a lot for printf
  }
  // // If optimized
  // {
  //   gt_go(g, 32); // O2 => Need even less space
  //   for (int i = 0; i < 10; ++i)
  //     gt_go(f, 4096); // Need a lot for printf
  // }
  gt_exit(0);
}
