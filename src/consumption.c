#include <stdio.h>
#include "runtime.c"

#ifdef NDEBUG
const size_t PRODUCER_STACK_SIZE = 0x100;
const size_t CONSUMER_STACK_SIZE = 0x1000;
#else
const size_t PRODUCER_STACK_SIZE = 0x100000;
const size_t CONSUMER_STACK_SIZE = 0x100000;
#endif

const size_t PRODUCTIONS = 10;
const size_t PRODUCERS = 10;

size_t n = 0;
gt_ch ch;

void producer(void) {
  for (int i = 0; i < PRODUCTIONS; ++i) {
    gt_write(ch, ch + i);
    gt_yield();
  }
}

void consumer(void) {
  while (n < PRODUCTIONS * PRODUCERS) {
    printf("%lu ", gt_read(ch) - ch);
    gt_yield();
    ++n;
  }
  puts("");
}

int main(void) {
  gt_init();
  ch = gt_chan();
  for (int i = 0; i < PRODUCERS; ++i)
    gt_go(producer, PRODUCER_STACK_SIZE);
  gt_go(consumer, CONSUMER_STACK_SIZE);
  gt_exit(0);
}
