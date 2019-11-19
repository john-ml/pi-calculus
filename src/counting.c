#include <stdio.h>
#include "runtime.c"

const size_t PRODUCER_STACK_SIZE = 0x1000; // Need a lot for printf("%d %d ", ..)

#ifdef NPRINTFDEBUG
  #ifdef OPT2_FLAG
    const size_t CONSUMER_STACK_SIZE = 0x20; // No printf + optimizations
  #else
    const size_t CONSUMER_STACK_SIZE = 0x80; // No printf
  #endif
#else
  const size_t CONSUMER_STACK_SIZE = 0x1000;
#endif

static int n = 0;

void g(void) {
  while (n != 100)
    gt_yield();
  puts("");
}

void f(void) {
  for (int i = 0; i < 10; ++i) {
    printf("%d %d ", i, ++n);
    gt_yield();
  }
}

int main(void) {
  gt_init();
  gt_go(g, CONSUMER_STACK_SIZE); 
  for (int i = 0; i < 10; ++i)
    gt_go(f, PRODUCER_STACK_SIZE); // Need a lot for printf
  gt_exit(0);
}
