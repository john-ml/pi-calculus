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
  asm ("jmp gt_stop\t\n");
}

void f(void) {
  uint64_t *rsp = (uint64_t *)gt_self()->rsp + 1;
  uint64_t local1 = rsp[0];
  uint64_t local2 = rsp[1];
  printf("local1 = %lu, local2 = %lu\n", local1, local2);
  for (int i = 0; i < 10; ++i) {
    printf("%d %d ", i, ++n);
    gt_yield();
  }
  asm ("jmp gt_stop\t\n");
}

int main(void) {
  gt_init();
  gt_go(g, CONSUMER_STACK_SIZE); 
  for (int i = 0; i < 10; ++i) {
    gt_t t = gt_go_alloca(f, 16, PRODUCER_STACK_SIZE); // Need a lot for printf
    uint64_t *rsp = (uint64_t *)t->rsp + 1;
    rsp[0] = 2*i;
    rsp[1] = 2*i + 1;
  }
  gt_exit(0);
}
