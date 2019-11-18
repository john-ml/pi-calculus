.section .rodata

hi:
  .string "hi\n"

.section .text

.equ POOL_SIZE, 0x1000000000

.equ PROT_READ, 1
.equ PROT_WRITE, 2
.equ MAP_PRIVATE, 2
.equ MAP_ANONYMOUS, 0x20
.equ PROT, PROT_READ | PROT_WRITE
.equ MAP, MAP_PRIVATE | MAP_ANONYMOUS

mmalloc:
  mov %rdi, %rsi
  xor %rdi, %rdi
  mov $PROT, %rdx
  mov $MAP, %rcx
  mov $-1, %r8
  mov $0, %r9
  sub $8, %rsp
  call mmap@plt
  add $8, %rsp
  ret

.globl main
main:
  lea hi(%rip), %rdi
  sub $8, %rsp
  xor %eax, %eax
  call printf@plt
  mov $POOL_SIZE, %rdi
  call mmalloc
  add $8, %rsp
  xor %eax, %eax
  ret
