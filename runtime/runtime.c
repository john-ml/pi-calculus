// ---------------------------- Hyperparameters --------------------------------

// #define NDEBUG // no asserts
#define NPRINTFDEBUG // no debug logs
#define GT_CHAN_SIZE 0x10 // size of channel ring buffer

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
gt_t gt_go(void f(void), size_t n);

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

// Destroy a channel
void gt_drop(gt_ch c);

// Read from a channel
gt_val gt_read(gt_ch c);

// Write v to a channel
void gt_write(gt_ch c, gt_val v);

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
} gt_ctx;

typedef struct gt_queue {
  gt_t front, back;
} *gt_queue;

// Channel state: (waiting to write, ring buffer, waiting to read)
struct gt_chan {
  // IDLE threads are stored in read/write queues
  struct gt_queue readers, writers;
  // Values are stored in a ring buffer
  size_t first, last;
  gt_val values[GT_CHAN_SIZE];
  // Free list
  gt_ch next;
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
void gt_switch_asm(gt_t old, gt_t new);

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

gt_t gt_go(void f(void), size_t n) {
  gt_t t;
  if (threads_free) {
    t = threads_free;
    threads_free = threads_free->next;
  } else
    t = threads_end++;
  memset(t, 0, sizeof(*t));
  // Set up thread stack
  t->sp = !t->sp ? malloc(n) : realloc(t->sp, n);
  assert(t->sp && "gt_go: failed to allocate stack");
  t->rsp = &t->sp[n - 16];
  *(uint64_t *)&t->sp[n - 8] = (uint64_t)gt_stop;
  *(uint64_t *)&t->sp[n - 16] = (uint64_t)f;
  gt_queue_enq(&threads_on, t);
  return t;
}

gt_t gt_self(void) { return current; }

bool gt_yield(void) {
  if (gt_queue_empty(&threads_on))
    return false;
  gt_t t = current;
  gt_queue_enq(&threads_on, t);
  gt_switch_asm(t, current = gt_queue_deq(&threads_on));
  return true;
}

void gt_idle(void) {
  gt_t t = current;
  gt_switch_asm(t, current = gt_queue_deq(&threads_on));
}

void gt_stop(void) {
  assert(!gt_queue_empty(&threads_on) && "gt_stop: empty queue");
  gt_t t = current;
  t->next = threads_free;
  threads_free = t;
  debugf("%lu: STOP\n", t - threads);
  gt_dump();
  gt_switch_asm(t, current = gt_queue_deq(&threads_on));
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

static bool gt_ch_empty(gt_ch c) { return c->first == c->last; }

static bool gt_ch_full(gt_ch c) { return gt_ch_succ(c->last) == c->first; }

static gt_val gt_ch_deq(gt_ch c) {
  assert(!gt_ch_empty(c) && "gt_ch_deq: reading from empty queue");
  gt_val v = c->values[c->first];
  c->first = gt_ch_succ(c->first);
  return v;
}

static void gt_ch_enq(gt_ch c, gt_val v) {
  assert(!gt_ch_full(c) && "gt_ch_enq: writing to full queue");
  c->values[c->last] = v;
  c->last = gt_ch_succ(c->last);
}

// ---------------------------------- Channel ----------------------------------

// Create a new channel
gt_ch gt_chan(void) {
  gt_ch c;
  if (channels_free) {
    c = channels_free;
    channels_free = channels_free->next;
  } else
    c = channels_end++;
  memset(c, 0, sizeof(*c));
  return c;
}

// Destroy a channel
void gt_drop(gt_ch c) {
  c->next = channels_free;
  channels_free = c;
}

gt_val gt_read(gt_ch c) {
  while (gt_ch_empty(c)) {
    gt_queue_enq(&c->readers, current);
    debugf("%lu: WAIT gt_read(%lu)\n", current - threads, c - channels);
    gt_dump();
    gt_idle();
  }
  gt_ch r = gt_ch_deq(c);
  if (!gt_queue_empty(&c->writers))
    gt_queue_enq(&threads_on, gt_queue_deq(&c->writers));
  debugf("%lu: gt_read(%lu) = %lu\n",
    current - threads,
    c - channels,
    r - channels);
  gt_dump();
  return (gt_val)r;
}

void gt_write(gt_ch c, gt_val v) {
  while (gt_ch_full(c)) {
    gt_queue_enq(&c->writers, current);
    debugf("%lu: WAIT gt_write(%lu, %lu)\n",
      current - threads,
      c - channels,
      v - channels);
    gt_dump();
    gt_idle();
  }
  gt_ch_enq(c, v);
  if (!gt_queue_empty(&c->readers))
    gt_queue_enq(&threads_on, gt_queue_deq(&c->readers));
  debugf("%lu: gt_write(%lu, %lu)\n",
    current - threads, c - channels, v - channels);
  gt_dump();
}

void gt_dump(void) {
  debugf("  threads (%lu total):\n", threads_end - threads);
  debugf("    ON: %lu", current - threads);
  for (gt_t t = threads_on.front; t; t = t->next)
    debugf(" %lu", t - threads);
  debugs("");
  debugf("  channels (%lu total):\n", channels_end - channels);
  for (size_t i = 0; i < channels_end - channels; ++i) {
    debugf("    %lu: %s\n", i, !channels[i].next ? "ON" : "OFF");
    debugf("      readers: ");
    for (gt_t id = channels[i].readers.front; id; id = id->next)
      debugf("%lu ", id - threads);
    debugs("");
    debugf("      values: ");
    for (size_t j = channels[i].first; j != channels[i].last; j = gt_ch_succ(j))
      debugf("%lu ", channels[i].values[j] - channels);
    debugs("");
    debugf("      writers: ");
    for (gt_t id = channels[i].writers.front; id; id = id->next)
      debugf("%lu ", id - threads);
    debugs("");
  }
}
