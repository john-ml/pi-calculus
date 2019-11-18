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
gt_t gt_get(void);

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

// ---------------------------- Hyperparameters --------------------------------

#define GT_CHAN_SIZE 0x8

// -------------------- Context and channel data structures --------------------

// Thread state
typedef enum gt_st {GT_OFF = 0, GT_ON, GT_IDLE} gt_st;

// An execution context
typedef struct gt_ctx {
  char *rsp;
  uint64_t rbx, rbp, r12, r13, r14, r15;
  char *sp; // Thread stack
  size_t stack_size; 
  gt_t next; // Used in read/write queues
  gt_st st;
} gt_ctx;

typedef struct gt_queue {
  gt_t front, back;
} gt_queue;

// Channel state: (waiting to write, ring buffer, waiting to read)
struct gt_chan {
  // Read/write queues are linked lists
  gt_queue readers, writers;
  // Values are stored in a ring-buffer
  size_t first, last;
  gt_ch values[GT_CHAN_SIZE];
  bool on;
};

// ------------------------------ Global state ---------------------------------

// Local variables
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
  c->rsp = &c->sp[n - 16];
  *(uint64_t *)&c->sp[n - 8] = (uint64_t)gt_stop;
  *(uint64_t *)&c->sp[n - 16] = (uint64_t)f;
  c->st = GT_ON;
}

gt_t gt_get(void) { return current; }

// Compute next thread in pool (with wraparound)
static gt_t gt_next(gt_t c) {
  return c + 1 >= threads_end ? threads : c + 1;
}

bool gt_yield(void) {
  //puts("--------------------\ngt_yield()");
  //gt_dump();
  // Find next ON thread
  gt_ctx *old_current = current, *c = gt_next(current);
  while (c != current && c->st != GT_ON)
    c = gt_next(c);
  if (c == current)
    return false; // No other ON threads
  // Switch to the new thread
  current = c;
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
  exit(c);
}

// // --------------------------- Read/write queue --------------------------------
// 
// bool gt_queue_empty(gt_queue *q) { return !q->front; }
// 
// gt_id_t gt_queue_dequeue(gt_queue *q) {
//   assert(!gt_queue_empty(q) && "gt_queue_dequeue: dequeueing from empty queue");
//   gt_id_t r = q->front;
//   q->front = gt_get(r)->next;
//   return r;
// }
// 
// void gt_queue_enqueue(gt_queue *q, gt_id_t i) {
//   if (gt_queue_empty(q))
//     q->front = q->back = i;
//   else
//     q->back = gt_get(q->back)->next = i;
// }
// 
// // ------------------------------ Ring buffer ----------------------------------
// 
// static size_t gt_chan_succ(size_t i) { return (i + 1ul) % GT_CHAN_SIZE; }
// 
// bool gt_chan_empty(gt_chan *c) { return c->first == c->last; }
// 
// bool gt_chan_full(gt_chan *c) { return gt_chan_succ(c->last) == c->first; }
// 
// gt_chan *gt_chan_get(size_t i) { return &channels[i - 1]; }
// 
// gt_val gt_chan_dequeue(gt_chan *c) {
//   assert(!gt_chan_empty(c) && "gt_chan_dequeue: reading from empty queue");
//   gt_val v = c->values[c->first];
//   c->first = gt_chan_succ(c->first);
//   return v;
// }
// 
// void gt_chan_enqueue(gt_chan *c, gt_val v) {
//   assert(!gt_chan_full(c) && "gt_chan_enqueue: writing to full queue");
//   c->values[c->last] = v;
//   c->last = gt_chan_succ(c->last);
// }
// 
// // ---------------------------------- Channel ----------------------------------
// 
// // Create a new channel
// gt_chan_t gt_chan_new(void) {
//   // Find unused channel...
//   gt_chan *c = channels;
//   while (c < &channels[n_channels] && c->on)
//     ++c;
//   // ...and expand channel pool if necessary
//   if (c == &channels[n_channels]) {
//     size_t m = 2*n_channels;
//     channels = realloc(channels, m*sizeof(gt_chan));
//     memset(c = &channels[n_channels], 0, n_channels*sizeof(gt_chan));
//     n_channels = m;
//   } else
//     memset(c, 0, sizeof(gt_chan));
//   assert(!c->on && "gt_chan_new: clobbering used channel");
//   return (gt_chan_t)(c - channels) + 1ul;
// }
// 
// // The invariant to preserve when reading/writing from channels:
// // At any given moment there are some number of ON threads able to read/write
// // from the channel. Suppose there are m threads writing and n threads reading.
// // Then m + n < the capacity of the ring buffer, |ring buffer| >= n, and if
// // both wait queues are nonempty then m + n = the capacity of the ring buffer
// // and |ring buffer| = n.
// 
// // Reading from a channel on thread r:
// //   (ws       , xs       , non-empty rs) ~~> (ws, xs, rs ++ [r]), r IDLE
// //   (ws       , []       , []          ) ~~> (ws, [], [r]      ), r IDLE
// //   ([]       , xs ++ [x], []          ) ~~> ([], xs, []       ), cont r(x)
// //   (ws ++ [w], xs ++ [x], []          ) ~~> (ws, xs, []       ), cont r(x), w ON
// gt_val gt_chan_read(gt_chan_t c) {
//   printf("----------------------------\ngt_chan_read(%lu)\n", c);
//   gt_dump();
//   gt_chan *ch = gt_chan_get(c);
//   if (!gt_queue_empty(&ch->readers) || gt_chan_empty(ch)) {
//     gt_queue_enqueue(&ch->readers, gt_id());
//     current->st = GT_IDLE;
//     gt_yield();
//   }
//   // We need to call gt_chan_get again because channels might've been resized
//   ch = gt_chan_get(c);
//   if (!gt_queue_empty(&ch->writers))
//     gt_get(gt_queue_dequeue(&ch->writers))->st = GT_ON;
//   return gt_chan_dequeue(ch);
// }
// 
// // Writing x to a channel on thread w:
// //   (non-empty ws, xs         , rs       ) ~~> ([w] ++ ws, xs         , rs), w IDLE
// //   ([]          , FULL       , rs       ) ~~> ([w]      , FULL       , rs), w IDLE
// //   ([]          , non-full xs, [r] ++ rs) ~~> ([]       , [x] ++ [xs], rs), r ON, cont w
// //   ([]          , non-full xs, []       ) ~~> ([]       , [x] ++ xs  , []), cont w
// void gt_chan_write(gt_chan_t c, gt_val v) {
//   printf("----------------------------\ngt_chan_write(%lu, %lu)\n", c, v);
//   gt_dump();
//   gt_chan *ch = gt_chan_get(c);
//   if (!gt_queue_empty(&ch->writers) || gt_chan_full(ch)) {
//     gt_queue_enqueue(&ch->writers, gt_id());
//     current->st = GT_IDLE;
//     gt_yield();
//   }
//   ch = gt_chan_get(c);
//   if (!gt_queue_empty(&ch->readers))
//     gt_get(gt_queue_dequeue(&ch->readers))->st = GT_ON;
//   gt_chan_enqueue(ch, v);
// }
// 
// void gt_dump(void) {
//   puts("----------- Threads -----------");
//   for (size_t i = 0; i < n_threads; ++i) {
//     printf("%lu: ", i);
//     switch (threads[i].st) {
//       case GT_ON: printf("ON"); break;
//       case GT_OFF: printf("OFF"); break;
//       case GT_IDLE: printf("IDLE"); break;
//     }
//     puts(i == current - threads ? " (ACTIVE)" : "");
//   }
//   puts("---------- Channels -----------");
//   for (size_t i = 0; i < n_channels; ++i) {
//     printf("%lu: %s\n", i, channels[i].on ? "ON" : "OFF");
//     printf("  readers: ");
//     for (gt_id_t id = channels[i].readers.front; id; id = gt_get(id)->next)
//       printf("%lu ", id);
//     puts("");
//     printf("  values: ");
//     for (size_t j = channels[i].first; j != channels[i].last; j = gt_chan_succ(j))
//       printf("%lu ", channels[i].values[j]);
//     puts("");
//     printf("  writers: ");
//     for (gt_id_t id = channels[i].writers.front; id; id = gt_get(id)->next)
//       printf("%lu ", id);
//     puts("");
//   }
// }
