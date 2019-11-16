#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// Initialize thread pool.
void gt_init(void);

// Spawn a new thread.
void gt_go(void f(void));

// Yield execution to another thread.
// Return true if switched to from another thread.
// Return false if there were no other threads to switch to.
bool gt_yield(void);

// Stop execution of current thread.
// Equivalent to returning from the thread.
void gt_stop(void);

// Wait for all threads to terminate and exit with error code c.
// Can only be called from bootstrap thread.
void gt_exit(int c);

// An execution context.
typedef struct ctx ctx;

// Stash old context and switch to new.
void gt_switch(ctx *old, ctx *new);

// -----------------------------------------------------------------------------

typedef struct ctx {
  char *rsp;
  uint64_t rbx, rbp, r12, r13, r14, r15;
  char *st;
  bool used;
} ctx;

static const size_t STACK_SIZE = 0x400000;
static size_t n_threads = 8;
static ctx *threads; // Thread pool. &threads[0] is the bootstrap thread.
static ctx *current; // Currently executing thread.

void gt_init(void) {
  threads = calloc(n_threads, sizeof(ctx));
  (current = threads)->used = true;
}

void gt_go(void f(void)) {
  // Find unused thread...
  ctx *c = threads;
  while (c < &threads[n_threads] && c->used)
    ++c;
  // ...and expand thread pool if necessary
  if (c == &threads[n_threads]) {
    size_t m = 2*n_threads;
    threads = realloc(threads, m*sizeof(ctx));
    memset(&threads[n_threads], 0, n_threads*sizeof(ctx));
    c = &threads[n_threads];
    n_threads = m;
  }
  assert(!c->used);
  // Set up thread stack
  if (!c->st)
    c->st = malloc(STACK_SIZE);
  c->rsp = &c->st[STACK_SIZE - 16];
  *(uint64_t *)&c->st[STACK_SIZE - 8] = (uint64_t)gt_stop;
  *(uint64_t *)&c->st[STACK_SIZE - 16] = (uint64_t)f;
  c->used = true;
}

// Compute next thread in pool (with wraparound)
static ctx *next(ctx *c) {
  return (c + 1 == &threads[n_threads]) ? threads : c + 1;
}

bool gt_yield(void) {
  // Find next used thread
  ctx *old_current = current, *c = next(current);
  while (c != current && !c->used)
    c = next(c);
  if (c == current)
    return false; // No other used threads
  // Switch to the new thread
  current = c;
  gt_switch(old_current, current);
  return true;
}

void gt_stop(void) {
  current->used = false;
  gt_yield();
  assert(!"reachable"); 
}

void gt_exit(int c) {
  assert(current == threads);
  while (gt_yield())
    ;
  exit(c);
}
