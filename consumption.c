#include <stdio.h>
#include "runtime.c"

gt_chan_t ch;

void producer(void) {
  for (int i = 0; i < 100; ++i) {
    gt_chan_write(ch, (unsigned long)i);
    //gt_yield();
  }
}

void consumer(void) {
  for (;;) {
    printf("%lu\n", gt_chan_read(ch));
    //gt_yield();
  }
}

int main(void) {
  gt_init();
  ch = gt_chan_new();
  gt_go(producer);
  gt_go(producer);
  gt_go(consumer);
  gt_exit(0);
}
