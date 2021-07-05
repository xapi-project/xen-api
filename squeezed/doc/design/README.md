Squeezed: a host memory ballooning daemon for Xen
=================================================

Squeezed is a single host memory ballooning daemon. It helps by:

1.  allowing VM memory to be adjusted dynamically without having to reboot;
    and

2.  avoiding wasting memory by keeping everything fully utilised, while retaining
    the ability to take memory back to start new VMs.

Squeezed currently includes a simple
[Ballooning policy](#ballooning-policy)
which serves as a useful default.
The policy is written with respect to an abstract
[Xen memory model](#the-memory-model), which is based
on a number of
[assumptions about the environment](#environmental-assumptions),
for example that most domains have co-operative balloon drivers.
In theory the policy could be replaced later with something more sophisticated
(for example see
[xenballoond](https://github.com/avsm/xen-unstable/blob/master/tools/xenballoon/xenballoond.README)).

The [Toolstack interface](#toolstack-interface) is used by
[Xenopsd](https://github.com/xapi-project/xenopsd) to free memory
for starting new VMs.
Although the only known client is Xenopsd,
the interface can in theory be used by other clients. Multiple clients
can safely use the interface at the same time.

The [internal structure](#the-structure-of-the-daemon) consists of
a single-thread event loop. To see how it works end-to-end, consult
the [example](#example-operation).

No software is ever perfect; to understand the flaws in Squeezed,
please consult the
[list of issues](#issues).

Environmental assumptions
=========================

1.  The Squeezed daemon runs within a Xen domain 0 and
    communicates to xenstored via a Unix domain socket. Therefore
    Squeezed
    is granted full access to xenstore, enabling it to modify every
    domain’s `memory/target`.

2.  The Squeezed daemon calls
    `setmaxmem` in order to cap the amount of memory a domain can use.
    This relies on a patch to
    [xen which allows `maxmem` to be set lower than `totpages`](http://xenbits.xen.org/xapi/xen-3.4.pq.hg?file/c01d38e7092a/max-pages-below-tot-pages).
    See Section [maxmem](#use-of-maxmem) for more information.

3.  The Squeezed daemon
    assumes that only domains which write `control/feature-balloon` into
    xenstore can respond to ballooning requests. It will not ask any
    other domains to balloon.

4.  The Squeezed daemon
    assumes that the memory used by a domain is: (i) that listed in
    `domain_getinfo` as `totpages`; (ii) shadow as given by
    `shadow_allocation_get`; and (iii) a small (few KiB) of
    miscellaneous Xen structures
    (e.g. for domains, vcpus) which are invisible.

5.  The Squeezed daemon
    assumes that a domain which is created with a particular
    `memory/target` (and `startmem`, to within rounding error) will
    reach a stable value of `totpages` before writing
    `control/feature-balloon`. The daemon writes this value to
    `memory/memory-offset` for future reference.

    -   The Squeezed daemon
        does not know or care exactly what causes the difference between
        `totpages` and `memory/target` and it does *not*
        expect it to remain constant across Xen releases. It
        only expects the value to remain constant over the lifetime of a
        domain.

6.  The Squeezed daemon
    assumes that the balloon driver has hit its target when difference
    between `memory/target` and `totpages` equals the `memory-offset`
    value.

    -   Corrollary: to make a domain with a responsive balloon driver
        currenty using `totpages` allocate or free
        *x*
        it suffices to
        set `memory/target` to
        *x+`totpages`-`memoryoffset`*
        and wait for the
        balloon driver to finish. See Section [memory model](#the-memory-model) for more
        detail.

7.  The Squeezed daemon must
    maintain a “slush fund” of memory (currently 9MiB) which it must
    prevent any domain from allocating. Since (i) some Xen operations (such
    as domain creation) require memory within a physical address range
    (e.g. less than 4GiB) and (ii) since Xen preferentially
    allocates memory outside these ranges, it follows that by preventing
    guests from allocating *all* host memory (even
    transiently) we guarantee that memory from within these special
    ranges is always available. Squeezed operates in
    [two phases](#twophase-section): first causing memory to be freed; and
    second causing memory to be allocated.

8.  The Squeezed daemon
    assumes that it may set `memory/target` to any value within range:
    `memory/dynamic-max` to `memory/dynamic-min`

9.  The Squeezed daemon
    assumes that the probability of a domain booting successfully may be
    increased by setting `memory/target` closer to `memory/static-max`.

10. The Squeezed daemon
    assumes that, if a balloon driver has not made any visible progress
    after 5 seconds, it is effectively *inactive*. Active
    domains will be expected to pick up the slack.

# Toolstack interface

The toolstack interface introduces the concept of a *reservation*.
A *reservation* is: an amount of host free memory tagged
with an associated *reservation id*. Note this is an
internal Squeezed concept and Xen is
completely unaware of it. When the daemon is moving memory between
domains, it always aims to keep

![host free memory >= s + sum_i(reservation_i)](http://xapi-project.github.io/squeezed/doc/design/hostfreemem.svg)

where *s* is the size of the “slush fund” (currently 9MiB) and
![reservation_t](http://xapi-project.github.io/squeezed/doc/design/reservation.svg)
is the amount corresponding to the *i*th
reservation.

As an aside: Earlier versions of Squeezed always
associated memory with a Xen domain. Unfortunately
this required domains to be created before memory was freed which was
problematic because domain creation requires small amounts of contiguous
frames. Rather than implement some form of memory defragmentation,
Squeezed and Xenopsd were
modified to free memory before creating a domain. This necessitated
making memory *reservations* first-class stand-alone
entities.

Once a *reservation* is made (and the corresponding memory
is freed), it can be *transferred* to a domain created by a
toolstack. This associates the *reservation* with that
domain so that, if the domain is destroyed, the
*reservation* is also freed. Note that Squeezed is careful not
to count both a domain’s *reservation* and its `totpages`
during e.g. domain building: instead it considers the domain’s
allocation to be the maximum of *reservation* and
`totpages`.

The size of a *reservation* may either be specified exactly
by the caller or the caller may provide a memory range. If a range is
provided the daemon will allocate at least as much as the minimum value
provided and as much as possible up to the maximum. By allocating as
much memory as possible to the domain, the probability of a successful
boot is increased.

Clients of the Squeezed provide a string
name when they log in. All untransferred reservations made by a client
are automatically deleted when a client logs in. This prevents memory
leaks where a client crashes and loses track of its own reservation ids.

The interface looks like this:

    string session_id login(
      string client_name
    )

    string reservation_id reserve_memory(
      string client_name,
      int kib
    )

    int amount, string reservation_id reserve_memory_range(
      string client_name,
      int min,
      int max
    )

    void delete_reservation(
      string client_name,
      string reservation_id
    )

    void transfer_reservation_to_domain(
      string client_name,
      string reservation_id,
      int domid
    )

[The Xenopsd code](https://github.com/xapi-project/xenopsd/blob/bf4f8d13ded299b56e55a4b36221ada3dfa0b2b1/xc/xenops_server_xen.ml#L353) in pseudocode works as follows:

     r_id = reserve_memory_range("xenopsd", min, max);
     try:
        d = domain_create()
        transfer_reservation_to_domain("xenopsd", r_id, d)
     with:
        delete_reservation("xenopsd", r_id)

The interface is currently implemented using a trivial RPC protocol
over a Unix domain socket in domain 0.

Ballooning policy
=================

This section describes the very simple default policy currently
built-into Squeezed.

Every domain has a pair of values written into xenstore:
`memory/dynamic-min` and `memory/dynamic-max` with the following
meanings:

- `memory/dynamic-min` the lowest value that Squeezed is allowed
  to set `memory/target`. The administrator should make this as low as
  possible but high enough to ensure that the applications inside the
  domain actually work.
- `memory/dynamic-max`
  the highest value that Squeezed is allowed
  to set `memory/target`. This can be used to dynamically cap the
  amount of memory a domain can use.

If all balloon drivers are responsive then Squeezed daemon allocates
memory proportionally, so that each domain has the same value of:
![target-min/(max-min)](http://xapi-project.github.io/squeezed/doc/design/fraction.svg)

So:

-   if memory is plentiful then all domains will have
    `memory/target`=`memory/dynamic-max`

-   if memory is scarce then all domains will have
    `memory/target`=`memory/dynamic-min`

Note that the values of `memory/target` suggested by the policy are
ideal values. In many real-life situations (e.g. when a balloon driver
fails to make progress and is declared *inactive*) the
`memory/target` values will be different.

Note that, by default, domain 0 has
`memory/dynamic-min`=`memory/dynamic-max`, effectively disabling
ballooning. Clearly a more sophisticated policy would be required here
since ballooning down domain 0 as extra domains are started would be
counterproductive while backends and control interfaces remain in
domain 0.

The memory model
================

Squeezed
considers a ballooning-aware domain (i.e. one which has written
the `feature-balloon` flag into xenstore) to be completely described by
the parameters:

- `dynamic-min`: policy value written to `memory/dynamic-min` in xenstore by a
  toolstack (see Section [Ballooning policy](#ballooning-policy))

- `dynamic-max`: policy value written to `memory/dynamic-max` in xenstore by a
    toolstack (see Section [Ballooning policy](#ballooning-policy))

- `target`: balloon driver target written to `memory/target` in xenstore by
   Squeezed.

- `totpages`: instantaneous number of pages used by the domain as returned by
  the hypercall `domain_getinfo`

- `memory-offset`: constant difference between `target` and `totpages` when the
  balloon driver believes no ballooning is necessary: where
  `memory-offset` = `totpages` - `target` when the balloon driver believes it
  has reached its target.

- `maxmem`: upper limit on `totpages`: where `totpages` <= `maxmem`

For convenience we define a `adjusted-target` to be the *target* value necessary
to cause a domain currently using `totpages` to maintain this value
indefinitely so `adjusted-target` = `totpages` - `memory-offset`.

The Squeezed
daemon believes that:

-   a domain should be ballooning iff
    `adjusted-target` <> `target` (unless it has become *inactive*)

-   a domain has hit its target iff
    `adjusted-target` = `target` (to within 1 page);

-   if a domain has
    `target` = `x` then, when ballooning
    is complete, it will have
    `totpages` = `memory-offset` + `x`; and therefore

-   to cause a domain to free `y` it sufficies to set
    `target` := `totpages` - `memory-offset` - `y`.

The Squeezed
daemon considers non-ballooning aware domains (i.e. those which have not
written `feature-balloon`) to be represented by pairs of:

- `totpages`: instantaneous number of pages used by the domain as returned by
    `domain_getinfo`

- `reservation`: memory initially freed for this domain by Squeezed after a
  `transfer_reservation_to_domid` call

Note that non-ballooning aware domains will always have
`startmem` = `target`
since the domain will not be
instructed to balloon. Since a domain which is being built will have
0 <= `totpages` <= `reservation`, Squeezed computes
![unused(i)=reservation(i)-totpages](http://xapi-project.github.io/squeezed/doc/design/unused.svg)
and subtracts this from its model of the host’s free memory, ensuring
that it doesn’t accidentally reallocate this memory for some other
purpose.

The Squeezed
daemon believes that:

-   all guest domains start out as non-ballooning aware domains where
    `target`=`reservation`=`startmem`$;

-   some guest domains become ballooning-aware during their boot
    sequence i.e. when they write `feature-balloon`

The Squeezed
daemon considers a host to be represented by:

- ballooning domains: a set of domains which Squeezed will instruct
  to balloon;

- other domains: a set of booting domains and domains which have no
  balloon drivers (or whose balloon drivers have failed)

- a "slush fund" of low memory required for Xen

- `physinfo.free_pages` total amount of memory instantanously free
  (including both `free_pages` and `scrub_pages`)

- reservations: batches of free memory which are not (yet) associated
  with any domain

The Squeezed
daemon considers memory to be unused (i.e. not allocated for any useful
purpose) if it is neither in use by a domain nor reserved.

The main loop
=============

The main loop is triggered by either:

1.  the arrival of an allocation request on the toolstack interface; or

2.  the policy engine – polled every 10s – deciding that a target
    [adjustment is needed](https://github.com/xapi-project/squeezed/blob/7a5601d1543bd27e1e390a0a4f0a50aa531760e6/src/memory_server.ml#L60).

Each iteration of the main loop generates the following actions:

1.  Domains which were active but have failed to make progress towards
    their target in 5s are declared *inactive*. These
    domains then have:
    `maxmem` set to the minimum of `target` and `totpages.

2.  Domains which were inactive but have started to make progress
    towards their target are declared *active*. These
    domains then have: `maxmem` set to `target`.

3.  Domains which are currently active have new targets computed
    according to the policy (see Section [Ballooning policy](#ballooning-policy)). Note that
    inactive domains are ignored and not expected to balloon.

Note that domains remain classified as *inactive* only
during one run of the main loop. Once the loop has terminated all
domains are optimistically assumed to be *active* again.
Therefore should a domain be classified as *inactive* once,
it will get many later chances to respond.

The targets are set in [two phases](#two-phase-target-setting).
The [maxmem](#use-of-maxmem) is used to prevent domains suddenly allocating
more memory than we want them to.

The main loop has a notion of a host free memory “target”, similar to
the existing domain memory `target`. When we are trying to free memory
(e.g. for starting a new VM), the host free memory “target” is
increased. When we are trying to distribute memory among guests
(e.g. after a domain has shutdown and freed lots of memory), the host
free memory “target” is low. Note the host free memory “target” is
always at least several MiB to ensure that some host free memory with
physical address less than 4GiB is free (see [Two phase target setting](#two-phase-target-setting) for
related information).

The main loop terminates when all *active* domains have
reached their targets (this could be because all domains responded or
because they all wedged and became inactive); and the policy function
hasn’t suggested any new target changes. There are three possible
results:

1.  Success if the host free memory is near enough its “target”;

2.  Failure if the operation is simply impossible within the policy
    limits (i.e. `dynamic_min` values are too high;

3.  Failure if the operation failed because one or more domains became
    *inactive* and this prevented us from reaching our host
    free memory “target”.

Note that, since only *active* domains have their targets
set, the system effectively rewards domains which refuse to free memory
(*inactive*) and punishes those which do free memory
(*active*). This effect is countered by signalling to the
admin which domains/VMs aren’t responding so they can take corrective
action. To achieve this, the daemon monitors the list of
*inactive* domains and if a domain is
*inactive* for more than 20s it writes a flag into xenstore
`memory/uncooperative`. This key can be monitored and used to generate
an alert, if desired.

Two phase target setting
------------------------

The following diagram shows how a system with two domains can evolve if domain
`memory/target` values are increased for some domains and decreased for
others, at the same time. Each graph shows two domains (domain 1 and
domain 2) and a host. For a domain, the square box shows its
`adjusted-totpages` and the arrow indicates the direction of the
`memory/target`. For the host the square box indicates total free
memory. Note the highlighted state where the host’s free memory is
temporarily exhausted

![Two phase target setting](http://xapi-project.github.io/squeezed/doc/design/twophase.svg)

In the
initial state (at the top of the diagram), there are two domains, one
which has been requested to use more memory and the other requested to
use less memory. In effect the memory is to be transferred from one
domain to the other. In the final state (at the bottom of the diagram),
both domains have reached their respective targets, the memory has been
transferred and the host free memory is at the same value it was
initially. However the system will not move atomically from the initial
state to the final: there are a number of possible transient in-between
states, two of which have been drawn in the middle of the diagram. In
the left-most transient state the domain which was asked to
*free* memory has freed all the memory requested: this is
reflected in the large amount of host memory free. In the right-most
transient state the domain which was asked to *allocate*
memory has allocated all the memory requested: now the host’s free
memory has hit zero.

If the host’s free memory hits zero then Xen  has been forced to
give all memory to guests, including memory less than 4GiB which is critical
for allocating certain structures. Even if we ask a domain to free
memory via the balloon driver there is no guarantee that it will free
the *useful* memory. This leads to an annoying failure mode
where operations such as creating a domain free due to `ENOMEM` despite
the fact that there is apparently lots of memory free.

The solution to this problem is to adopt a two-phase `memory/target`
setting policy. The Squeezed daemon forces
domains to free memory first before allowing domains to allocate,
in-effect forcing the system to move through the left-most state in the
diagram above.

Use of maxmem
-------------

The Xen
domain `maxmem` value is used to limit memory allocations by the domain.
The rules are:

1.  if the domain has never been run and is paused then
    `maxmem` is set to `reservation (reservations were described
    in the [Toolstack interface](#toolstack-interface) section above);

    -   these domains are probably still being built and we must let
        them allocate their `startmem`

    -   **FIXME**: this "never been run" concept pre-dates the
        `feature-balloon` flag: perhaps we should use the
        `feature-balloon` flag instead.

2.  if the domain is running and the balloon driver is thought to be
    working then `maxmem` is set to `target`; and

    -   there may be a delay between lowering a target and the domain
        noticing so we prevent the domain from allocating memory when it
        should in fact be deallocating.

3.  if the domain is running and the balloon driver is thought to be
    inactive then
    `maxmem` is set to the minimum of `target` and `actual`.

    -   if the domain is using more memory than it should then we allow
        it to make progress down towards its target; however

    -   if the domain is using less memory than it should then we must
        prevent it from suddenly waking up and allocating more since we
        have probably just given it to someone else

    -   **FIXME**: should we reduce the target to leave the domain in a
        neutral state instead of asking it to allocate and fail forever?

Example operation
=================

The diagram shows an initial system state comprising 3 domains on a
single host. The state is not ideal; the domains each have the same
policy settings (`dynamic-min` and `dynamic-max`) and yet are using
differing values of `adjusted-totpages`. In addition the host has more
memory free than desired. The second diagram shows the result of
computing ideal target values and the third diagram shows the result
after targets have been set and the balloon drivers have
responded.

![calculation](http://xapi-project.github.io/squeezed/doc/design/calculation.svg)

The scenario above includes 3 domains (domain 1,
domain 2, domain 3) on a host. Each of the domains has a non-ideal
`adjusted-totpages` value.

Recall we also have the policy constraint that:
`dynamic-min` <= `target` <= `dynamic-max`
Hypothetically if we reduce `target` by
`target`-`dynamic-min` (i.e. by setting
`target` to `dynamic-min`) then we should reduce
`totpages` by the same amount, freeing this much memory on the host. In
the upper-most graph in the diagram above, the total amount of memory
which would be freed if we set each of the 3 domain’s
`target` to `dynamic-min` is:
`d1` + `d2` + `d3`. In this hypothetical
situation we would now have
`x` + `s` + `d1` + `d2` + `d3` free on the host where
`s` is the host slush fund and `x` is completely unallocated. Since we
always want to keep the host free memory above $s$, we are free to
return `x` + `d1` + `d2` + `d3` to guests. If we
use the default built-in proportional policy then, since all domains
have the same `dynamic-min` and `dynamic-max`, each gets the same
fraction of this free memory which we call `g`:
![definition of g](http://xapi-project.github.io/squeezed/doc/design/g.svg)
For each domain, the ideal balloon target is now
`target` = `dynamic-min` + `g`.
Squeezed does not set all the targets at once: this would allow the
allocating domains to race with the deallocating domains, potentially allowing
all low memory to be allocated. Therefore Squeezed sets the
targets in [two phases](#two-phase-target-setting).

The structure of the daemon
===========================

Squeezed is a single-threaded daemon which is started by an `init.d`
script. It sits waiting for incoming requests on its toolstack interface
and checks every 10s whether all domain targets are set to the ideal
values
(recall the [Ballooning policy](#ballooning-policy)). If an allocation request
arrives or if the domain targets require adjusting then it calls into
the module
[squeeze_xen.ml](https://github.com/xapi-project/squeezed/blob/master/src/squeeze_xen.ml).

The module
[src/squeeze_xen.ml](https://github.com/xapi-project/squeezed/blob/master/src/squeeze_xen.ml)
contains code which inspects the state of the host (through hypercalls
and reading xenstore) and creates a set of records describing the
current state of the host and all the domains. Note this snapshot of
state is not atomic – it is pieced together from multiple hypercalls and
xenstore reads – we assume that the errors generated are small and we
ignore them. These records are passed into the
[squeeze.ml](https://github.com/xapi-project/squeezed/blob/master/lib/squeeze.ml)
module where they
are processed and converted into a list of *actions* i.e.
(i) updates to `memory/target` and; (ii) declarations that particular
domains have become *inactive* or *active*.
The rationale for separating the Xen interface from the
main ballooning logic was to make testing easier: the module
[test/squeeze_test.ml](https://github.com/xapi-project/squeezed/blob/master/test/squeeze_test.ml)
contains a simple simulator which allows various edge-cases to be
checked.

Issues
======

-   If a linux domU kernel has the netback, blkback or blktap modules
    then they away pages via `alloc_empty_pages_and_pagevec()` during
    boot. This interacts with the balloon driver to break the assumption
    that, reducing the target by `x` from a neutral value should free
    `x` amount of memory.

-   Polling the state of the host (particular the xenstore contents) is
    a bit inefficient. Perhaps we should move the policy values
    `dynamic_min` and `dynamic_max` to a separate place in the xenstore
    tree and use watches instead.

-   The memory values given to the domain builder are in units of MiB.
    We may wish to similarly quantise the `target` value or check that
    the `memory-offset` calculation still works.

-   The Xen
    patch queue reintroduces the lowmem emergency pool. This was an
    attempt to prevent guests from allocating lowmem before we switched
    to a two-phase target setting procedure. This patch can probably be
    removed.

-   It seems unnecessarily evil to modify an *inactive*
    domain’s `maxmem` leaving `maxmem` less than `target}``, causing
    the guest to attempt allocations forwever. It’s probably neater to
    move the `target` at the same time.

-   Declaring a domain *active* just because it makes small
    amounts of progress shouldn’t be enough. Otherwise a domain could
    free 1 byte (or maybe 1 page) every 5s.

-   Likewise, declaring a domain “uncooperative” only if it has been
    *inactive* for 20s means that a domain could alternate
    between *inactive* for 19s and *active*
    for 1s and not be declared “uncooperative”.

Document history
----------------

Version | Date          | Change
--------|---------------|---------------------
0.2     | 10th Nov 2014 | Update to markdown
0.1     | 9th Nov 2009  | Initial version
