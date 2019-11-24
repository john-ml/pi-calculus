# π-calculus

Emits C code and leaves the rest to gcc.

`cabal run` to build and view command line options.

Features:
- Green threads for concurrency, based on
    [this tutorial](https://c9x.me/articles/gthreads/code0.html).
- Channels for communication: channels contain a small buffer to allow for 
    multiple threads to make progress even if they all write to the same channel.
    Attempting to read from an empty buffer/write to a full buffer adds the
    current thread to a wait queue, and the waiting thread won't be scheduled until
    it's able to make progress again.
- Register allocation: gcc is good at this, but stack locals that need
    to be kept alive across forks have to be explicitly copied from one thread's stack
    to another's. All callee-save registers are reserved for π-calculus locals and
    a naive heuristic tells the allocator to avoid spilling variables that are live
    across many forks.

Repo map:
- [src](https://github.com/johnli0135/pi-calculus/tree/master/src):
    the compiler.
- [runtime](https://github.com/johnli0135/pi-calculus/tree/master/runtime):
    C implementation of green threads and channels for communication.
- [examples](https://github.com/johnli0135/pi-calculus/tree/master/examples):
    some example `pi` programs.
- [scrap](https://github.com/johnli0135/pi-calculus/tree/master/scrap):
    various experiments in C and assembly.
- [sandbox](https://github.com/johnli0135/pi-calculus/blob/master/sandbox):
    misc. notes / todo items.
