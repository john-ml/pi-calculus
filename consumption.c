#include <stdio.h>
#include "runtime.c"

#ifdef NDEBUG
  #define PRODUCER_STACK_SIZE 0x100
  #define CONSUMER_STACK_SIZE 0x1000
#else
  #define PRODUCER_STACK_SIZE 0x100000
  #define CONSUMER_STACK_SIZE 0x100000
#endif

gt_ch ch;

void producer(void) {
  for (int i = 0; i < 10; ++i) {
    gt_write(ch, ch + i);
    gt_yield();
  }
}

void consumer(void) {
  for (;;) {
    printf("%lu\n", gt_read(ch) - ch);
    gt_yield();
  }
}

int main(void) {
  gt_init();
  ch = gt_chan();
  gt_go(producer, PRODUCER_STACK_SIZE);
  gt_go(producer, PRODUCER_STACK_SIZE);
  gt_go(consumer, CONSUMER_STACK_SIZE);
  gt_exit(0);
}
