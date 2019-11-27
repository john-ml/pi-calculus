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

const size_t PRODUCTIONS = 10;
const size_t PRODUCERS = 10;
const size_t CONSUMERS = 10;

size_t n = 0;
gt_ch ch;
bool done = false;

void producer(void) {
  for (int i = 0; i < PRODUCTIONS; ++i) {
    gt_write(ch, ch + i);
    gt_yield();
  }
  asm ("jmp gt_stop\t\n");
}

struct handler_extra consumer_extra;
void consumer(void) {
  for (;;) {
    printf("%lu ", (gt_ch)consumer_extra.info - channels);
    gt_t t = current;
    gt_switch(t, current = consumer_extra.cause);
  }
}

int main(void) {
  gt_init();
  ch = gt_write_only_chan(consumer, &consumer_extra, CONSUMER_STACK_SIZE);
  for (int i = 0; i < PRODUCERS; ++i)
    gt_go(producer, PRODUCER_STACK_SIZE);
  gt_exit(0);
}
