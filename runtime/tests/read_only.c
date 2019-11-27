#include <stdio.h>
#include "runtime.c"

const size_t CONSUMER_STACK_SIZE = 0x1000; // printf("%lu ", ...) needs lots of mem

/*
#ifdef NPRINTFDEBUG
  #ifdef OPT2_FLAG
    const size_t PRODUCER_STACK_SIZE = 0x40;
  #else
    const size_t PRODUCER_STACK_SIZE = 0x100;
  #endif
#else */
  const size_t PRODUCER_STACK_SIZE = 0x1000;
/*#endif */

const size_t CONSUMPTIONS = 10;
const size_t CONSUMERS = 10;

size_t n = 0;
gt_ch ch;
bool done = false;

struct handler_extra producer_extra;
uint64_t i = 0;
void producer(void) {
  for (;;) {
    *(uint64_t *)producer_extra.info = i++;
    gt_t t = current;
    gt_switch(t, current = producer_extra.cause);
  }
}

void consumer(void) {
  for (int i = 0; i < CONSUMPTIONS; ++i) {
    printf("%lu ", (uint64_t)gt_read(ch));
    gt_yield();
  }
  asm ("jmp gt_stop\t\n");
}

int main(void) {
  gt_init();
  ch = gt_read_only_chan(producer, &producer_extra, PRODUCER_STACK_SIZE);
  for (int i = 0; i < CONSUMERS; ++i)
    gt_go(consumer, CONSUMER_STACK_SIZE);
  gt_exit(0);
}
