#include <stdlib.h>
#include "runtime.c"
gt_ch global10;
void var_r3(gt_ch c) {
  printf("%d", (int)(long)c);
}
void var_h4(void) {
  //for (;;) {
    var_r3(gt_read(global10));
  //  gt_yield();
  //}
  asm ("jmp gt_stop\t\n");
}
gt_ch global9;
gt_ch var_w5(void) {
  puts("stdin_i32 body: Reading in user input...");
  int n = 0;
    int c;
    do {c = getchar();
    }while (c == '\n' || c == ' ');
    while ('0' <= c && c <= '9') {n = 10*n + (c - '0');
      c = getchar();
    }printf("stdin_i32 body: Got %d from the user\n", n);
    return (gt_ch)(long)n;
  asm ("jmp gt_stop\t\n");
}
void var_h6(void) {
  for (;;) {
    puts("stdin_i32: Waiting for user input...");
    gt_ch res = var_w5();
    printf("stdin_i32: Got %d from the user. Writing to stdin_i32...\n", (int)(long)res);
    gt_write(global9, res);
    puts("stdin_i32: Waiting for next read from std_i32.");
    gt_wait_read(global9);
  }
  asm ("jmp gt_stop\t\n");
}
gt_ch global8;
void var_r7(gt_ch ch) {
  printf("adder body: got ch = %lu\n", ch - channels);
  puts("adder body: Waiting for first operand...");
  uint64_t fst = (uint64_t)gt_read(ch);
  puts("adder body: Waiting for second operand...");
  uint64_t snd = (uint64_t)gt_read(ch);
  printf("adder body: Got operands %lu, %lu\n", fst, snd);
  puts("adder body: Writing sum to private channel...");
  gt_write(ch, (gt_ch)(fst + snd));
  puts("adder body: Done!");
  asm ("jmp gt_stop\t\n");
}
void var_h8(void) {
  //for (;;) {
    puts("adder: Waiting for private channel...");
    var_r7(gt_read(global8));
    puts("adder: Waiting for write to adder.");
  //  gt_wait_write(global8);
  //}
  asm ("jmp gt_stop\t\n");
}
void var_f0(void) {
  global8 = gt_chan();
  gt_go(var_h8, 0x10000);
  global9 = gt_chan();
  gt_go(var_h6, 0x10000);
  global10 = gt_chan();
  gt_go(var_h4, 0x10000);
  puts("main: Waiting for first int...");
  r15 = gt_read(global9);
  puts("main: Waiting for second int...");
  rbx = gt_read(global9);
  printf("main: Got ints: %d, %d\n", (int)(long)r15, (int)(long)rbx);
  puts("main: Creating private channel...");
  r12 = gt_chan();
  puts("main: Sending private channel to adder...");
  gt_write(global8, r12);
  puts("main: Sending operand 1 to adder...");
  gt_write(r12, r15);
  puts("main: Sending operand 2 to adder...");
  gt_write(r12, rbx);
  puts("main: Waiting for result from adder...");
  r15 = gt_read(r12);
  printf("main: Got result: %d\n", (int)(long)r15);
  gt_write(global10, r15);
  asm ("jmp gt_stop\t\n");
}
int main(void) {
  gt_init();
  gt_t var_t1 = gt_go(var_f0, 0x100000);
  gt_exit(0);
}
