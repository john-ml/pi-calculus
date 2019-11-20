#include <stdint.h>
#include <stdio.h>

typedef unsigned __int128 u128;

const uint64_t P = (uint64_t)-59;

uint64_t mul(uint64_t x, uint64_t y) {
  return ((u128)x * (u128)y) % P;
}

uint64_t recip(uint64_t x) {
  return (x + 1) % P;
}

int main() {
  printf("%lu\n", mul((uint64_t)-1, (uint64_t)-1));
}
