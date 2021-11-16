Functions to execute processes, pass file descriptors around and return results

Why does this exist?
====================


The low-level Unix.fork(), Unix.exec*() functions and friends are not safe to 
call in multithreaded programs for two reasons:

  + parallel threads opening new file descriptors will have these descriptors
    captured by a fork(). This leads to annoying glitches like (for example)
    attempts to 'umount' a filesystem being rejected because a file is still
    open.

  + although Unix.fork() will call (via the ocaml runtime) a pthread_atfork
    handler which attempts to clean up the state of the threading system in
    the child, this relies on quite a complex glibc implementation which has
    been observed to fail occasionally (typical symptom is the process spins
    after the fork() but before the exec())

Additionally Unix.fork(), Unix.exec*() are very low-level primitives. When we
call these functions what we actually want to do is run some separate process
with certain file-descriptors, optionally returning results. 

The interface in this module
    
  + is higher-level than Unix.fork(), Unix.exec*()
  + allows us to offload Unix.fork(), Unix.exec*() to a single-threaded
    separate process where the glibc+ocaml runtime codepaths are simpler and
    hopefully more reliable.
