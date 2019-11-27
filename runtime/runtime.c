// ---------------------------- Hyperparameters --------------------------------

// #define NDEBUG // no asserts
#define NPRINTFDEBUG // no debug logs
#define GT_CHAN_SIZE 0x2 // size of channel ring buffer
#define INLINE_STACK_SIZE 128 // max size of thread stack that doesn't need malloc

// -----------------------------------------------------------------------------

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>

// A green thread
typedef struct gt_ctx *gt_t;

// Initialize thread pool
void gt_init(void);

// Spawn a new thread with n-byte stack
// f must jmp gt_stop instead of returning.
gt_t gt_go(void f(void), size_t n);

// Spawn a new thread with n-byte stack and initial m-byte alloca.
// i.e., initial stack is
//                    .
//                    .
//   &f               .
//   .                .  n bytes
//   .   m bytes      .
//   .                .
// f must jmp gt_stop instead of returning.
gt_t gt_go_alloca(void f(void), size_t m, size_t n);

// Current thread
gt_t gt_self(void);

// Yield execution to another thread
// Return true if switched to from another thread
// Return false if there were no other threads to switch to
bool gt_yield(void);

// Stop execution of current thread
void gt_stop(void);

// Wait for all threads to terminate and exit with error code c
// Can only be called from bootstrap thread
void gt_exit(int c);

// A communication channel
typedef struct gt_chan *gt_ch;

// Channels are values in pi-calculus
typedef gt_ch gt_val;

// Create a new channel
gt_ch gt_chan(void);

struct handler_extra;

// Register "on read" event for ch. Everyone loses the ability to write to ch.
// Future calls to gt_read(ch) will resume the handler before attempting to read
// from ch.
gt_ch gt_read_only_chan(void h(void), struct handler_extra *extra, size_t n);

// Register "on write" event for ch. Everyone loses the ability to read from ch.
// Future calls to gt_write(ch, v) will resume the handler with extra->info = v
// instead of actually writing anything to the channel.
gt_ch gt_write_only_chan(void h(void), struct handler_extra *extra, size_t n);

// Destroy a channel
void gt_drop(gt_ch c);

// Read from a channel
gt_val gt_read(gt_ch c);

// Write v to a channel
void gt_write(gt_ch c, gt_val v);

// Request to idle until someone reads from ch
void gt_wait_read(gt_ch ch);

// Request to idle until someone writes to ch
void gt_wait_write(gt_ch ch);

// An event handler for interfacing with C code.
typedef struct {
  gt_t t; // A thread with enough stack space to handle the event with normal C
  // Extra info the handler needs to work
  struct handler_extra {
    void *info; // An extra value
    gt_t cause; // The thread which triggered the event
  } *extra;
} handler;

// Dump thread + channel state
// NB: Uses printf a lot so, make sure you have >= 4096 bytes of stack space.
void gt_dump(void);

// -------------------- Context and channel data structures --------------------

// An execution context
typedef struct gt_ctx {
  char *rsp;
  void *rbx, *rbp, *r12, *r13, *r14, *r15;
  char *sp; // Thread stack
  gt_t next; // Used in read/write queues, OFF list, ON queue
  char _[8]; // To ensure stack is 16-aligned
  char stack[INLINE_STACK_SIZE]; // Inline stack
} gt_ctx;

typedef struct gt_queue {
  gt_t front, back;
} *gt_queue;


struct gt_chan {
  // Free list
  gt_ch next;
  enum gt_ch_state {GT_CH_NORMAL, GT_CH_READ_ONLY, GT_CH_WRITE_ONLY} tag;
  union {
    // Standard channel state: (waiting to write, ring buffer, waiting to read)
    struct {
      // IDLE threads are stored in read/write queues
      struct gt_queue readers, writers;
      // Values are stored in a ring buffer
      size_t first, last;
      gt_val values[GT_CHAN_SIZE];
    } normal;
    // Read-only state: handler for on_read is expected to deposit its result in res
    struct {
      handler handler;
      gt_val res;
    } read_only;
    // Write-only state: every write just resumes the handler
    handler write_only;
  } as;
};

// ------------------------------ Global state ---------------------------------

// Local variables are callee saved registers when possible
register void *rbx asm ("rbx");
register void *r12 asm ("r12");
register void *r13 asm ("r13");
register void *r14 asm ("r14");
register void *r15 asm ("r15");

// Thread pool
static gt_t threads; // &threads[0] is the bootstrap thread
static gt_t threads_end;
static gt_t current; // currently executing thread
static struct gt_queue threads_on = {NULL, NULL}; // ON threads form a queue
static gt_t threads_free; // OFF threads form a free list

// Channel
static gt_ch channels;
static gt_ch channels_end;
static gt_ch channels_free; // OFF channels form a free list

// ------------------------------ For debugging ---------------------------------

#ifdef NPRINTFDEBUG

#define debugf(...)
#define debugs(_)

#else

#define debugf printf
#define debugs puts

#endif

// ---------------------------------- Queue ------------------------------------

static bool gt_queue_empty(gt_queue q) { return !q->front; }

static gt_t gt_queue_deq(gt_queue q) {
  assert(!gt_queue_empty(q) && "gt_queue_deq: dequeueing from empty queue");
  gt_t r = q->front;
  q->front = r->next;
  return r;
}

static void gt_queue_enq(gt_queue q, gt_t t) {
  t->next = NULL;
  if (gt_queue_empty(q))
    q->front = q->back = t;
  else
    q->back = q->back->next = t;
}

// ---------------------------------- Threads ----------------------------------

// Stash old context and switch to new
void gt_switch(gt_t old, gt_t new);

// Assume result is 8-aligned
static void *mmalloc(size_t bytes) {
  void *p = MAP_FAILED;
  while (p == MAP_FAILED) {
    p = mmap(
      NULL, bytes,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS,
      -1, 0);
    bytes -= 1ull << 16;
  }
  debugf("Acquired %llu bytes with mmalloc.\n", bytes + (1ull << 16));
  assert(p != MAP_FAILED && "mmalloc: mmap failed");
  return p;
}

void gt_init(void) {
  // mmap 4GB and split the address space in half
  // Lower half for threads...
  threads = current = mmalloc(1ull << 32);
  memset(current, 0, sizeof(*current));
  threads_end = threads + 1;
  threads_free = NULL;
  // ...and upper half for channels
  channels = channels_end = (struct gt_chan *)((char *)threads + (1ull << 31));
  channels_free = NULL;
}

gt_t gt_go(void f(void), size_t n) { return gt_go_alloca(f, 0, n); }

static gt_t gt_spawn_alloca(void f(void), size_t m, size_t n) {
  gt_t t;
  if (threads_free) {
    t = threads_free;
    threads_free = threads_free->next;
  } else
    t = threads_end++;
  memset(t, 0, sizeof(*t));
  // Set up thread stack
  if (n > INLINE_STACK_SIZE)
    t->sp = t->sp && t->sp != t->stack ? realloc(t->sp, n) : malloc(n);
  else
    t->sp = t->stack;
  assert(t->sp && "gt_go: failed to allocate stack");
  *(void **)(t->rsp = &t->sp[n - 8 - m]) = f;
  return t;
}

static gt_t gt_spawn(void f(void), size_t n) { return gt_spawn_alloca(f, 0, n); }

gt_t gt_go_alloca(void f(void), size_t m, size_t n) {
  gt_t t = gt_spawn_alloca(f, m, n);
  gt_queue_enq(&threads_on, t);
  return t;
}

gt_t gt_self(void) { return current; }

bool gt_yield(void) {
  if (gt_queue_empty(&threads_on))
    return false;
  gt_t t = current;
  gt_queue_enq(&threads_on, t);
  gt_switch(t, current = gt_queue_deq(&threads_on));
  return true;
}

void gt_idle(void) {
  gt_t t = current;
  gt_switch(t, current = gt_queue_deq(&threads_on));
}

void gt_stop(void) {
  assert(!gt_queue_empty(&threads_on) && "gt_stop: empty queue");
  gt_t t = current;
  t->next = threads_free;
  threads_free = t;
  debugf("%lu: STOP\n", t - threads);
  gt_dump();
  gt_switch(t, current = gt_queue_deq(&threads_on));
}

void gt_exit(int c) {
  assert(current == threads && "gt_exit: called by non-bootstrap thread");
  while (gt_yield())
    ;
  // TODO: What if there are IDLE threads?
  exit(c);
}

// ------------------------------ Ring buffer ----------------------------------

static size_t gt_ch_succ(size_t i) { return (i + 1ul) % GT_CHAN_SIZE; }

static bool gt_ch_empty(gt_ch c) { return c->as.normal.first == c->as.normal.last; }

static bool gt_ch_full(gt_ch c) {
  return gt_ch_succ(c->as.normal.last) == c->as.normal.first;
}

static gt_val gt_ch_deq(gt_ch c) {
  assert(!gt_ch_empty(c) && "gt_ch_deq: reading from empty queue");
  gt_val v = c->as.normal.values[c->as.normal.first];
  c->as.normal.first = gt_ch_succ(c->as.normal.first);
  return v;
}

static void gt_ch_enq(gt_ch c, gt_val v) {
  assert(!gt_ch_full(c) && "gt_ch_enq: writing to full queue");
  c->as.normal.values[c->as.normal.last] = v;
  c->as.normal.last = gt_ch_succ(c->as.normal.last);
}

// ---------------------------------- Channel ----------------------------------

static gt_ch gt_alloc(void) {
  gt_ch c;
  if (channels_free) {
    c = channels_free;
    channels_free = channels_free->next;
  } else
    c = channels_end++;
  memset(c, 0, sizeof(*c));
  return c;
}

// Create a new channel
gt_ch gt_chan(void) {
  gt_ch c = gt_alloc();
  c->tag = GT_CH_NORMAL;
  return c;
}

gt_ch gt_read_only_chan(void h(void), struct handler_extra *extra, size_t n) {
  gt_ch c = gt_alloc();
  c->tag = GT_CH_READ_ONLY;
  gt_t t = gt_spawn(h, n);
  c->as.read_only.handler.t = t;
  c->as.read_only.handler.extra = extra;
  return c;
}

gt_ch gt_write_only_chan(void h(void), struct handler_extra *extra, size_t n) {
  gt_ch c = gt_alloc();
  c->tag = GT_CH_WRITE_ONLY;
  gt_t t = gt_spawn(h, n);
  c->as.write_only.t = t;
  c->as.write_only.extra = extra;
  return c;
}

// Destroy a channel
void gt_drop(gt_ch c) {
  c->next = channels_free;
  channels_free = c;
}

void gt_wait_write(gt_ch c) {
  gt_queue_enq(&c->as.normal.readers, current);
  debugf("%lu: gt_wait_write(%lu)\n", current - threads, c - channels);
  gt_dump();
  gt_idle();
}

static void gt_wait_read_(gt_ch c, gt_val v) {
  gt_queue_enq(&c->as.normal.writers, current);
  debugf("%lu: gt_wait_read_(%lu, %lu)\n",
    current - threads,
    c - channels,
    v - channels);
  gt_dump();
  gt_idle();
}

void gt_wait_read(gt_ch ch) { gt_wait_read_(ch, NULL); }

gt_val gt_read(gt_ch c) {
  switch (c->tag) {
  case GT_CH_NORMAL:
    while (gt_ch_empty(c))
      gt_wait_write(c);
    gt_ch r = gt_ch_deq(c);
    if (!gt_queue_empty(&c->as.normal.writers))
      gt_queue_enq(&threads_on, gt_queue_deq(&c->as.normal.writers));
    debugf("%lu: gt_read(%lu) = %lu\n",
      current - threads,
      c - channels,
      r - channels);
    gt_dump();
    return (gt_val)r;
  case GT_CH_READ_ONLY:
    c->as.read_only.handler.extra->info = &c->as.read_only.res;
    gt_t t = c->as.read_only.handler.extra->cause = current;
    gt_switch(t, current = c->as.read_only.handler.t);
    return c->as.read_only.res;
  case GT_CH_WRITE_ONLY:
    assert(0 && "gt_read: reading from write-only channel");
    break;
  default: return NULL;
  }
}

void gt_write(gt_ch c, gt_val v) {
  switch(c->tag) {
  case GT_CH_NORMAL:
    while (gt_ch_full(c))
      gt_wait_read_(c, v);
    gt_ch_enq(c, v);
    if (!gt_queue_empty(&c->as.normal.readers))
      gt_queue_enq(&threads_on, gt_queue_deq(&c->as.normal.readers));
    debugf("%lu: gt_write(%lu, %lu)\n",
      current - threads, c - channels, v - channels);
    gt_dump();
    return;
  case GT_CH_READ_ONLY:
    assert(0 && "gt_write: writing to read-only channel");
    return;
  case GT_CH_WRITE_ONLY:
    c->as.write_only.extra->info = v;
    gt_t t = c->as.write_only.extra->cause = current;
    gt_switch(t, current = c->as.write_only.t);
    return;
  default: return;
  }
}

void gt_dump(void) {
  debugf("  threads (%lu total):\n", threads_end - threads);
  debugf("    ON: %lu", current - threads);
  for (gt_t t = threads_on.front; t; t = t->next)
    debugf(" %lu", t - threads);
  debugs("");
  debugf("  channels (%lu total):\n", channels_end - channels);
  for (size_t i = 0; i < channels_end - channels; ++i) {
    if (channels[i].tag == GT_CH_NORMAL) {
      debugf("    %lu: %s\n", i, !channels[i].next ? "ON" : "OFF");
      debugf("      readers: ");
      for (gt_t id = channels[i].as.normal.readers.front; id; id = id->next)
        debugf("%lu ", id - threads);
      debugs("");
      debugf("      values: ");
      for (size_t j = channels[i].as.normal.first;
           j != channels[i].as.normal.last;
           j = gt_ch_succ(j))
        debugf("%lu ", channels[i].as.normal.values[j] - channels);
      debugs("");
      debugf("      writers: ");
      for (gt_t id = channels[i].as.normal.writers.front; id; id = id->next)
        debugf("%lu ", id - threads);
      debugs("");
    }
  }
}
