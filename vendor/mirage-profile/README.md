## mirage-profile -- collect runtime profiling information in CTF format

This library can be used to trace execution of OCaml/Lwt programs (such as Mirage unikernels) at the level of Lwt threads.
The traces can be viewed using JavaScript or GTK viewers provided by [mirage-trace-viewer][] or processed by tools supporting the [Common Trace Format (CTF)][ctf].
Some example traces can be found in the blog post [Visualising an Asynchronous Monad](http://roscidus.com/blog/blog/2014/10/27/visualising-an-asynchronous-monad/).

Libraries can use the functions mirage-profile provides to annotate the traces with extra information.
When compiled against a normal version of Lwt, mirage-profile's functions are null-ops (or call the underlying untraced operation, as appropriate) and OCaml's cross-module inlining will optimise these calls away, meaning there should be no overhead in the non-profiling case.


## Use with Mirage

See http://openmirage.org/wiki/profiling for instructions.

## Recording traces manually

To record traces you need to pin a version of Lwt with tracing support (this provides the `lwt.tracing` findlib module):

    $ opam pin add lwt 'https://github.com/mirage/lwt.git#tracing'

This will cause mirage-profile and any programs using it to be recompiled with tracing enabled.

To trace a **Unix process**, use `MProf_unix.mmap_buffer` to write to an mmapped file:

    let () =
      let buffer = MProf_unix.mmap_buffer ~size:1000000 "trace.ctf" in
      let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
      MProf.Trace.Control.start trace_config

You'll also need to link with the `mirage-profile` and `mirage-profile-unix` libraries.

To begin tracing a **Xen unikernel**, create a buffer and call `MProf.Trace.Control.start`:

    let trace_pages = MProf_xen.make_shared_buffer ~size:1000000
    let () = 
      let buffer = trace_pages |> Io_page.to_cstruct |> Cstruct.to_bigarray in
      let trace_config = MProf.Trace.Control.make buffer MProf_xen.timestamper in
      MProf.Trace.Control.start trace_config

To share the buffer with dom0, do this somewhere in your initialisation code:

    MProf_xen.share_with (module Gnt.Gntshr) (module OS.Xs) ~domid:0 trace_pages

You'll also need to link with the `mirage-profile` and `mirage-profile-xen` libraries.

## Viewing traces

To view the trace you should, ideally, call `MProf.Trace.Control.stop` before reading the buffer to avoid race conditions, but in practice reading the trace at any time usually works.

If your program crashes, you can still read the trace buffer.
On Xen, you can ensure that the buffer doesn't disappear by adding these lines to your guest's config file:

    on_crash = 'preserve'
    on_poweroff = 'preserve'

[mirage-trace-viewer][] contains tools for saving and viewing traces, as well as a `metadata` description of the format, which allows the traces to be read using e.g. [babeltrace][].


## Recording extra trace data

Programs and libraries are encouraged to record extra useful information using the `MProf` module.
As using these functions generally has no overhead when a regular Lwt is used, there should be no need to use conditional compilation for this.
See the `MProf.Trace` and `MProf.Counter` modules for documentation about what can be recorded.


[ctf]: http://www.efficios.com/ctf
[babeltrace]: http://www.efficios.com/babeltrace
[mirage-trace-viewer]: https://github.com/talex5/mirage-trace-viewer
