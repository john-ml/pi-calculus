// ---------------------------- Hyperparameters --------------------------------

#define NDEBUG
#define GT_CHAN_SIZE 0x3

// -----------------------------------------------------------------------------

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
void gt_go(void f(void), size_t n);

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

// Write v to a channel
void gt_write(gt_ch c, gt_val v);

// Read from a channel
gt_val gt_read(gt_ch c);

// Dump thread/channel state
void gt_dump(void);

// -------------------- Context and channel data structures --------------------

// Thread state
typedef enum gt_st {GT_OFF = 0, GT_ON, GT_IDLE} gt_st;

// An execution context
typedef struct gt_ctx {
  char *rsp;
  uint64_t rbx, rbp, r12, r13, r14, r15;
  char *sp; // Thread stack
  gt_t next; // Used in read/write queues
  gt_st st;
} gt_ctx;

typedef struct gt_queue {
  gt_t front, back;
} *gt_queue;

// Channel state: (waiting to write, ring buffer, waiting to read)
struct gt_chan {
  // Read/write queues are linked lists
  struct gt_queue readers, writers;
  // Values are stored in a ring-buffer
  size_t first, last;
  gt_val values[GT_CHAN_SIZE];
  bool on;
};

// ------------------------------ Global state ---------------------------------

// Local variables are callee saved registers when possible
register void *rbx asm ("rbx");
register void *r12 asm ("r12");
register void *r13 asm ("r13");
register void *r14 asm ("r14");
register void *r15 asm ("r15");

// Thread pool
static struct gt_ctx *threads; // &threads[0] is the bootstrap thread
static struct gt_ctx *current; // Currently executing thread
static struct gt_ctx *threads_end;

// Channel
static struct gt_chan *channels;
static struct gt_chan *channels_end;

// ------------------------------ For debugging ---------------------------------

#ifdef NDEBUG

#define debugf(...)
#define debugs(_)

#else

#define debugf printf
#define debugs puts

#endif

// ---------------------------------- Threads ----------------------------------

// Stash old context and switch to new
void gt_switch(gt_ctx *old, gt_ctx *new);

// Assume result is 8-aligned
static void *mmalloc(size_t bytes) {
  return mmap(
    NULL, bytes,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANONYMOUS,
    -1, 0);
}

void gt_init(void) {
  threads = current = mmalloc(1ull << 32);
  channels = channels_end = (struct gt_chan *)((char *)threads + (1ull << 31));
  threads_end = current + 1;
  memset(current, 0, sizeof(gt_ctx));
  current->st = GT_ON;
}

void gt_go(void f(void), size_t n) {
  // Find OFF thread
  gt_ctx *c = threads;
  while (c < threads_end && c->st != GT_OFF)
    ++c;
  if (c == threads_end) {
    memset(c, 0, sizeof(gt_ctx));
    ++threads_end;
  }
  assert(!c->st && "gt_go: clobbering used thread");
  // Set up thread stack
  c->sp = !c->sp ? malloc(n) : realloc(c->sp, n);
  assert(c->sp && "gt_go: failed to allocate stack");
  c->rsp = &c->sp[n - 16];
  *(uint64_t *)&c->sp[n - 8] = (uint64_t)gt_stop;
  *(uint64_t *)&c->sp[n - 16] = (uint64_t)f;
  c->st = GT_ON;
}

gt_t gt_self(void) { return current; }

// Compute next thread in pool (with wraparound)
static gt_t gt_next(gt_t c) {
  return c + 1 >= threads_end ? threads : c + 1;
}

bool gt_yield(void) {
  // Find next ON thread
  gt_ctx *old_current = current, *c = gt_next(current);
  while (c != current && c->st != GT_ON)
    c = gt_next(c);
  if (c == current)
    return false; // No other ON threads
  // Switch to the new thread
  current = c;
  //debugf("%lu: gt_yield()\n", old_current - threads);
  //gt_dump();
  gt_switch(old_current, current);
  return true;
}

void gt_stop(void) {
  current->st = GT_OFF;
  gt_yield();
  assert(!"gt_stop: gt_yield returned"); 
}

void gt_exit(int c) {
  assert(current == threads && "gt_exit: called by non-bootstrap thread");
  while (gt_yield())
    ;
  // TODO: What if there are IDLE threads?
  exit(c);
}

// --------------------------- Read/write queue --------------------------------

static bool gt_queue_empty(gt_queue q) { return !q->front; }

static gt_t gt_queue_deq(gt_queue q) {
  assert(!gt_queue_empty(q) && "gt_queue_deq: dequeueing from empty queue");
  gt_t r = q->front;
  q->front = r->next;
  return r;
}

static void gt_queue_enq(gt_queue q, gt_t i) {
  i->next = NULL;
  if (gt_queue_empty(q))
    q->front = q->back = i;
  else
    q->back = q->back->next = i;
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
  // Find unused channel
  gt_ch c = channels;
  while (c < channels_end && c->on)
    ++c;
  if (c == channels_end) {
    memset(c, 0, sizeof(struct gt_chan));
    ++channels_end;
  }
  assert(!c->on && "gt_ch_new: clobbering used channel");
  c->on = true;
  return c;
}

// The invariant to preserve when reading/writing from channels:
// At any given moment there are some number of ON threads able to read/write
// from the channel. Suppose there are m threads writing and n threads reading.
// Then m + n < the capacity of the ring buffer, |ring buffer| >= n, and if
// both wait queues are nonempty then m + n = the capacity of the ring buffer
// and |ring buffer| = n.

// Reading from a channel on thread r:
//   (ws       , xs       , non-empty rs) ~~> (ws, xs, rs ++ [r]), r IDLE
//   (ws       , []       , []          ) ~~> (ws, [], [r]      ), r IDLE
//   ([]       , xs ++ [x], []          ) ~~> ([], xs, []       ), cont r(x)
//   (ws ++ [w], xs ++ [x], []          ) ~~> (ws, xs, []       ), cont r(x), w ON
gt_val gt_read(gt_ch c) {
  if (!gt_queue_empty(&c->readers) || gt_ch_empty(c)) {
    do {
      gt_queue_enq(&c->readers, current);
      current->st = GT_IDLE;
      debugf("%lu: WAIT gt_read(%lu)\n", current - threads, c - channels);
      gt_yield();
    } while (gt_ch_full(c));
  }
  if (!gt_queue_empty(&c->writers))
    gt_queue_deq(&c->writers)->st = GT_ON;
  gt_ch r = gt_ch_deq(c);
  debugf("%lu: gt_read(%lu) = %lu\n", current - threads, c - channels, r - channels);
  gt_dump();
  return r;
}

// Writing x to a channel on thread w:
//   (non-empty ws, xs         , rs       ) ~~> ([w] ++ ws, xs         , rs), w IDLE
//   ([]          , FULL       , rs       ) ~~> ([w]      , FULL       , rs), w IDLE
//   ([]          , non-full xs, [r] ++ rs) ~~> ([]       , [x] ++ [xs], rs), r ON, cont w
//   ([]          , non-full xs, []       ) ~~> ([]       , [x] ++ xs  , []), cont w
void gt_write(gt_ch c, gt_val v) {
  if (!gt_queue_empty(&c->writers) || gt_ch_full(c)) {
    do {
      gt_queue_enq(&c->writers, current);
      current->st = GT_IDLE;
      debugf("%lu: WAIT gt_write(%lu, %lu)\n",
        current - threads, c - channels, v - channels);
      gt_dump();
      gt_yield();
    } while (gt_ch_full(c));
  }
  if (!gt_queue_empty(&c->readers))
    gt_queue_deq(&c->readers)->st = GT_ON;
  gt_ch_enq(c, v);
  debugf("%lu: gt_write(%lu, %lu)\n",
    current - threads, c - channels, v - channels);
  gt_dump();
}

void gt_dump(void) {
  debugs("  threads:");
  for (size_t i = 0; i < threads_end - threads; ++i) {
    debugf("    %lu: ", i);
    switch (threads[i].st) {
      case GT_ON: debugf("ON"); break;
      case GT_OFF: debugf("OFF"); break;
      case GT_IDLE: debugf("IDLE"); break;
    }
    debugs(i == current - threads ? " (ACTIVE)" : "");
  }
  debugs("  channels:");
  for (size_t i = 0; i < channels_end - channels; ++i) {
    debugf("    %lu: %s\n", i, channels[i].on ? "ON" : "OFF");
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
