---
title: index
---
\+++ title = "Architecture" +++

Squeezed is responsible for managing the memory on a single host. Squeezed "balances" memory between VMs according to a policy written to Xenstore.

The following diagram shows the internals of Squeezed:

![Internals of squeezed](squeezed.png)

At the center of squeezed is an abstract model of a Xen host. The model includes:

- The amount of already-used host memory (used by fixed overheads such as Xen and the crash kernel).
- Per-domain memory policy specifically `dynamic-min` and `dynamic-max` which together describe a range, within which the domain's actual used memory should remain.
- Per-domain calibration data which allows us to compute the necessary balloon target value to achive a particular memory usage value.

Squeezed is a single-threaded program which receives commands from xenopsd over a Unix domain socket. When Xenopsd wishes to start a new VM, squeezed will be asked to create a "reservation". Note this is different to the Xen notion of a reservation. A squeezed reservation consists of an amount of memory squeezed will guarantee to keep free labelled with an id. When Xenopsd later creates the domain to notionally use the reservation, the reservation is "transferred" to the domain before the domain is built.

Squeezed will also wake up every 30s and attempt to rebalance the memory on a host. This is useful to correct imbalances caused by balloon drivers temporarily failing to reach their targets. Note that ballooning is fundamentally a co-operative process, so squeezed must handle cases where the domains refuse to obey commands.

The "output" of squeezed is a list of "actions" which include:

- Set domain x's `memory/target` to a new value.
- Set the `maxmem` of a domain to a new value (as a hard limit beyond which the domain cannot allocate).

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
