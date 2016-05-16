xenopsd: a simple VM manager
============================

xenopsd manages VMs running

  * on Xen, via direct libxc calls
  * on Xen/KVM via libvirt [experimental]
  * on KVM via qemu directly [experimental]

and provides a simple RPC control interface to the layer above (typically xapi).

## Coverage Profiling

This branch adds coverage profiling with bisect_ppx. Binaries write
`bisect*.out` files, when they exit to the current directory. These files
can be analysed with bisect-ppx-report:

    bisect-ppx-report -I _build -html coverage     *.out
    bisect-ppx-report -I _build -text coverage.txt *.out
    
## Location of Profiling Data

By default, an instrumented binary writes to `$CWD/bisect*.out`. In the
case of xenopsd in producition, this would be `/`. To avoid writing to `/`
and to avoid several services writing to the same place, start xenopsd
binaries with an evironment variable pointing to a better place:

    BISECT_FILE="/tmp/bisect-xenopsd"

Function `Bisect_setup.init name` sets the environment variable if it is
unset from inside the program such that log data is written to the temp
directory (respecting TMP and TEMP env vars if set). However, logging to
$CWD will still work even if the `init` function is not called.



