#include <stdio.h>

register void *rbx asm ("rbx");
register void *r12 asm ("r12");
register void *r13 asm ("r13");
register void *r14 asm ("r14");
register void *r15 asm ("r15");

int f(int i, int j, int k, int l, int m, int n, int o, int p) {
  return i + j + k + l + m + n + o + p;
}

int main() {
  rbx = main;
  int i, j, k, l, m, n, o, p;
  i = j = k = l = m = n = o = p = 0;
  f(i, j, k, l, m, n, o, p);
  printf("%p %p\n", main, rbx);
}
