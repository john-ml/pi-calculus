.globl _gt_switch_asm, gt_switch_asm
_gt_switch_asm:
gt_switch_asm:
  mov %rsp, 0x00(%rdi)
  mov %rbx, 0x08(%rdi)
  mov %rbp, 0x10(%rdi)
  mov %r12, 0x18(%rdi)
  mov %r13, 0x20(%rdi)
  mov %r14, 0x28(%rdi)
  mov %r15, 0x30(%rdi)
  mov 0x00(%rsi), %rsp
  mov 0x08(%rsi), %rbx
  mov 0x10(%rsi), %rbp
  mov 0x18(%rsi), %r12
  mov 0x20(%rsi), %r13
  mov 0x28(%rsi), %r14
  mov 0x30(%rsi), %r15
  ret
