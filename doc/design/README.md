Squeezed: a host memory ballooning daemon
=========================================

Version | Date          | Change
--------|---------------|---------------------
0.2     | 10th Nov 2014 | Update to markdown
0.1     | 9th Nov 2009  | Initial version

Introduction
============

We wish to:

1.  allow VM memory to be adjusted dynamically without having to reboot;
    and

2.  “squeeze” a few more VMs onto a host to cover the interval between
    another host failing and more capacity being brought online.

Squeezed is a
per-host memory ballooning daemon. It performs two tasks:

1.  it exports a simple host memory management interface to the
    [Xapi toolstack](https://github.com/xapi-project/xapi-project)
    toolstack through which [Xenopsd](https://github.com/xapi-project/xenopsd) can
    *reserve* memory for new domains;

2.  it applies a *ballooning policy* to all domains running
    on a host.

The daemon currently includes a simple ballooning policy (see
Section [Ballooning policy](#ballooning-policy) and the intention is that this can be
replaced later with more sophisticated policies
(e.g. *xenballoond*[^1])). Although the only client is the
Xapi toolstack, the interface can in theory be used by other clients.

The rest of this document is structured as follows.
Section [assumptions](#assumptions) lists assumptions made by the ballooning daemon on
other parts of the system; these assumptions need careful review and may
not be valid. Section [Toolstack interface](#toolstack-interface) describes the interface
between the toolstack and the ballooning daemon. Section
[Ballooning policy](#ballooning-policy) describes the simple built-in ballooning policy and
Section [memory model](#the-memory-model) describes how Squeezed models memory.
The main loop of the daemon is described in Section [The main loop](#the-main-loop) and
a detailed example is described in Section [example](#example).
Section [structure](#structure) describes the structure of the daemon itself and
finally Section [issues](#issues) lists some known issues.

Environmental assumptions
=========================

1.  The Squeezed daemon runs within a Xen domain 0 and
    communicates to xenstored via a Unix domain socket. Therefore
    Squeezed
    is granted full access to xenstore, enabling it to modify every
    domain’s `memory/target`.

2.  The Squeezed daemon calls
    `setmaxmem` in order to cap the amount of memory a domain can use.
    This relies on a patch to xen[^2] which allows `maxmem` to be set
    lower than `totpages`. See Section [maxmem](#use-of-maxmem) for more information.

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
    `control/feature-balloon`.[^3]The daemon writes this value to
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
        ![x](http://xapi-project.github.io/squeezed/doc/design/x.svg),
        it suffices to
        set `memory/target` to
        ![x+totpages-memoryoffset](http://xapi-project.github.io/squeezed/doc/design/xtotpages.svg)
        and wait for the
        balloon driver to finish. See Section [memory model](#the-memory-model) for more
        detail.

7.  The Squeezed daemon must
    maintain a “slush fund” of memory (currently 9MiB) which it must
    prevent any domain from allocating. Since (i) some Xen operations (such
    as domain creation) require memory within a physical address range
    (e.g. $<$ 4GiB) and (ii) since Xen preferentially
    allocates memory outside these ranges, it follows that by preventing
    guests from allocating *all* host memory (even
    transiently) we guarantee that memory from within these special
    ranges is always available. See Section [twophase section](#twophase-section) for more
    details.

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

This section begins by describing the concept of a
*reservation* and then describes the toolstack interface in
pseudocode.

A *reservation* is: an amount of host free memory tagged
with an associated *reservation id*. Note this is an
internal Squeezed concept and Xen is
completely unaware of it. When the daemon is moving memory between
domains, it always aims to keep
$$\mathit{host\ free\ memory} >= s + \sum_i{\mathit{reservation}_i}$$
where $s$ is the size of the “slush fund” (currently 9MiB) and
$\mathit{reservation}_i$ is the amount corresponding to the $i$th
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

    string session_id login(string client_name)

    string reservation_id reserve_memory(string client_name, int kib)
    int amount, string reservation_id reserve_memory_range(string client_name, int min, int max)

    void delete_reservation(string client_name, string reservation_id)

    void transfer_reservation_to_domain(string client_name, string reservation_id, int domid)

The Xapi
toolstack has code like the following: (in
<http://www.xen.org/files/XenCloud/ocamldoc/index.html?c=xapi&m=Vmops>)

     r_id = reserve_memory_range("xapi", min, max);
     try:
        d = domain_create()
        transfer_reservation_to_domain("xapi", r_id, d)
     with:
        delete_reservation("xapi", r_id)

The interface is currently implemented using a trivial RPC protocol over
xenstore where requests and responses are directories and their
parameters and return values are keys in those directories.

Ballooning policy
=================

This section describes the very simple default policy currently
built-into Squeezed.

Every domain has a pair of values written into xenstore:
`memory/dynamic-min` and `memory/dynamic-max` with the following
meanings:

`memory/dynamic-min`

:   : the lowest value that Squeezed is allowed
    to set `memory/target`. The administrator should make this as low as
    possible but high enough to ensure that the applications inside the
    domain actually work.

`memory/dynamic-max`

:   : the highest value that Squeezed is allowed
    to set `memory/target`. This can be used to dynamically cap the
    amount of memory a domain can use.

If all balloon drivers are responsive then Squeezed daemon allocates
memory proportionally, so that each domain has the same value of:
$$\frac{
\texttt{memory/target}-\texttt{memory/dynamic-min}
}{
\texttt{memory/dynamic-max}-\texttt{memory/dynamic-min}
}$$ So:

-   if memory is plentiful then all domains will have
    $\texttt{memory/target}=\texttt{memory/dynamic-max}$

-   if memory is scarce then all domains will have
    $\texttt{memory/target}=\texttt{memory/dynamic-min}$

Note that the values of `memory/target` suggested by the policy are
ideal values. In many real-life situations (e.g. when a balloon driver
fails to make progress and is declared *inactive*) the
`memory/target` values will be different.

Note that, by default, domain 0 has
$\texttt{dynamic\_min}=\texttt{dynamic\_max}$, effectively disabling
ballooning. Clearly a more sophisticated policy would be required here
since ballooning down domain 0 as extra domains are started would be…
problematic.

The memory model
================

This section describes the model used internally by Squeezed.

The Squeezed
daemon considers a ballooning-aware domain (i.e. one which has written
the `feature-balloon` flag into xenstore) to be a 6-tuple:
$$\mathit{ballooning~domain} = (\texttt{dynamic-min}, \texttt{dynamic-max}, \texttt{target}, \texttt{totpages}, \texttt{memory-offset}, \texttt{maxmem})$$
where

`dynamic-min`

:   : policy value written to `memory/dynamic-min` in xenstore by a
    toolstack (see Section [Ballooning policy](#ballooning-policy))

`dynamic-max`

:   : policy value written to `memory/dynamic-max` in xenstore by a
    toolstack (see Section [Ballooning policy](#ballooning-policy))

`target`

:   : balloon driver target written to `memory/target` in xenstore by Squeezed.

`totpages`

:   : instantaneous number of pages used by the domain as returned by
    `domain_getinfo`

`memory-offset`

:   : constant difference between `target` and `totpages` when the
    balloon driver believes no ballooning is necessary:
    $$\texttt{memory-offset} {\stackrel{def}{=}}\texttt{totpages} - \texttt{target} \mathit{~when~idle}$$

`maxmem`

:   : upper limit on `totpages`:
    $$\texttt{totpages} <= \texttt{maxmem}$$

For convenience we define a `totpages’` to be the target value necessary
to cause a domain currently using `totpages` to maintain this value
indefinitely.
$$\texttt{totpages'} {\stackrel{def}{=}}\texttt{totpages} - \texttt{memory-offset}$$

The Squeezed
daemon believes that:

-   a domain should be ballooning iff
    $\texttt{totpages'} <> \texttt{target}$ (unless it has become
    <span>*inactive*</span>);

-   a domain has hit its target iff
    $\texttt{totpages'} = \texttt{target}$ (to within 1 page);

-   if a domain has $\texttt{target}\leftarrow x$ then, when ballooning
    is complete, it will have
    $\texttt{totpages}=\texttt{memory-offset}+x$; and therefore

-   to cause a domain to free $y$ it sufficies to set
    $\texttt{target}\leftarrow\texttt{totpages}-\texttt{memory-offset}-y$.

The Squeezed
daemon considers non-ballooning aware domains (i.e. those which have not
written `feature-balloon`) to be represented by pairs of:
$$\mathit{other~domain} = (\texttt{totpages}, \mathit{reservation})$$
where

`totpages`

:   : instantaneous number of pages used by the domain as returned by
    `domain_getinfo`

$\mathit{reservation}$

:   : memory initially freed for this domain by Squeezed after a
    `transfer_reservation_to_domid` call

Note that non-ballooning aware domains will always have
$\texttt{startmem}=\texttt{target}$ since the domain will not be
instructed to balloon. Since a domain which is being built will have
$0<=\texttt{totpages}<=\mathit{reservation}$, Squeezed computes:
$$\mathit{unused}(i) {\stackrel{def}{=}}i.\mathit{reservation} - i.\texttt{totpages}$$
and subtracts this from its model of the host’s free memory, ensuring
that it doesn’t accidentally reallocate this memory for some other
purpose.

The Squeezed
daemon believes that:

-   all guest domains start out as non-ballooning aware domains where
    $\texttt{target}=\mathit{reservation}=\texttt{startmem}$;

-   some guest domains become ballooning-aware during their boot
    sequence i.e. when they write `feature-balloon`

The Squeezed
daemon considers a host to be a 5-tuple:
$$\mathit{host} = (\mathit{ballooning~domains}, \mathit{other~domains}, s, \texttt{physinfo.free\_pages}, \mathit{reservation}_i)$$
where

$\mathit{ballooning~domains}$

:   : a list of $\mathit{ballooning~domain}$ values representing domains
    which Squeezed will
    instruct to balloon;

$\mathit{other~domains}$

:   : a list of $\mathit{other~domain}$ values which includes both
    domains which are still booting and will transform into
    $\mathit{ballooning~domains}$ and those which have no balloon
    drivers.

$s$

:   : a “slush fund” of low memory required for Xen;

`physinfo.free_pages`

:   : total amount of memory instantanously free (including both
    `free_pages` and `scrub_pages`)

$\mathit{reservation}_i$

:   : a set of memory *reservations* not allocated to any
    domain

The Squeezed
daemon considers memory to be unused (i.e. not allocated for any useful
purpose) as follows:
$$\mathit{unused~memory} = \texttt{physinfo.free\_pages} -
\Sigma_i\mathit{reservation}_i - s - \Sigma_{i\in\mathit{other~domains}}\mathit{unused}(i)$$

The main loop
=============

The main loop [^4] is triggered by either:

1.  the arrival of an allocation request on the toolstack interface; or

2.  the policy engine – polled every 10s – deciding that a target
    adjustment is needed.

Each iteration of the main loop[^5] generates the following actions:

1.  Domains which were active but have failed to make progress towards
    their target in 5s are declared *inactive*. These
    domains then have:
    $$\texttt{maxmem}\leftarrow\mathit{min}(\texttt{target}, \texttt{totpages})$$

2.  Domains which were inactive but have started to make progress
    towards their target are declared *active*. These
    domains then have: $$\texttt{maxmem}\leftarrow\texttt{target}$$

3.  Domains which are currently active have new targets computed
    according to the policy (see Section [Ballooning policy](#ballooning-policy)). Note that
    inactive domains are ignored and not expected to balloon.

Note that domains remain classified as *inactive* only
during one run of the main loop. Once the loop has terminated all
domains are optimistically assumed to be *active* again.
Therefore should a domain be classified as *inactive* once,
it will get many later chances to respond.

See [Two phase target setting](#two-phase-target-setting)
for more detail on how targets are
updated and Section [maxmem] for more detail about `maxmem`.

The main loop has a notion of a host free memory “target”, similar to
the existing domain memory `target`. When we are trying to free memory
(e.g. for starting a new VM), the host free memory “target” is
increased. When we are trying to distribute memory among guests
(e.g. after a domain has shutdown and freed lots of memory), the host
free memory “target” is low. Note the host free memory “target” is
always at least several MiB to ensure that some host free memory with
physical address $<$ 4GiB is free (see [Two phase target setting](#two-phase-target-setting) for
related information).

The main loop terminates when all <span>*active*</span> domains have
reached their targets (this could be because all domains responded or
because they all wedged and became inactive); and the policy function
hasn’t suggested any new target changes. There are three possible
results:

1.  Success if the host free memory is near enough its “target”;

2.  Failure if the operation is simply impossible within the policy
    limits (i.e. `dynamic_min` values are too high;

3.  Failure if the operation failed because one or more domains became
    <span>*inactive*</span> and this prevented us from reaching our host
    free memory “target”.

Note that, since only <span>*active*</span> domains have their targets
set, the system effectively rewards domains which refuse to free memory
(<span>*inactive*</span>) and punishes those which do free memory
(<span>*active*</span>). This effect is countered by signalling to the
admin which domains/VMs aren’t responding so they can take corrective
action. To achieve this, the daemon monitors the list of
<span>*inactive*</span> domains and if a domain is
<span>*inactive*</span> for more than 20s it writes a flag into xenstore
`memory/uncooperative`. This key is seen by the <span><span
style="font-variant:small-caps;">XAPI</span></span> toolstack which
currently generates an alert to inform the admin.

Two phase target setting
------------------------

![The diagram shows how a system with two domains can evolve if domain
`memory/target` values are increased for some domains and decreased for
others, at the same time. Each graph shows two domains (domain 1 and
domain 2) and a host. For a domain, the square box shows its
$\texttt{totpages'}$ and the arrow indicates the direction of the
`memory/target`. For the host the square box indicates total free
memory. Note the highlighted state where the host’s free memory is
temporarily exhausted.](fig/twophase)

[twophase]

Consider the scenario shown graphically in Figure [twophase]. In the
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
<span>*free*</span> memory has freed all the memory requested: this is
reflected in the large amount of host memory free. In the right-most
transient state the domain which was asked to <span>*allocate*</span>
memory has allocated all the memory requested: now the host’s free
memory has hit zero.

If the host’s free memory hits zero then <span><span
style="font-variant:small-caps;">Xen</span></span> has been forced to
give all memory to guests, including memory $<$ 4GiB which is critical
for allocating certain structures. Even if we ask a domain to free
memory via the balloon driver there is no guarantee that it will free
the <span>*useful*</span> memory. This leads to an annoying failure mode
where operations such as creating a domain free due to `ENOMEM` despite
the fact that there is apparently lots of memory free.

The solution to this problem is to adopt a two-phase `memory/target`
setting policy. The <span><span
style="font-variant:small-caps;">squeezed</span></span> daemon forces
domains to free memory first before allowing domains to allocate,
in-effect forcing the system to move through the left-most state in the
diagram above.

Use of maxmem
-------------

The <span><span style="font-variant:small-caps;">Xen</span></span>
domain `maxmem` value is used to limit memory allocations by the domain.
The rules are:

1.  if the domain has never been run and is paused then
    $\texttt{maxmem}\leftarrow\texttt{reservation}$ (for information
    about reservations see Section [Toolstack interface](#toolstack-interface);

    -   these domains are probably still being built and we must let
        them allocate their `startmem`

    -   **FIXME**: this \`\`never been run’ concept pre-dates the
        `feature-balloon` flag: perhaps we should use the
        `feature-balloon` flag instead.

2.  if the domain is running and the balloon driver is thought to be
    working then $\texttt{maxmem}\leftarrow\texttt{target}$; and

    -   there may be a delay between lowering a target and the domain
        noticing so we prevent the domain from allocating memory when it
        should in fact be deallocating.

3.  if the domain is running and the balloon driver is thought to be
    inactive then
    $\texttt{maxmem}\leftarrow \mathit{min}(\texttt{target}, \texttt{actual})$.

    -   if the domain is using more memory than it should then we allow
        it to make progress down towards its target; however

    -   if the domain is using less memory than it should then we must
        prevent it from suddenly waking up and allocating more since we
        have probably just given it to someone else

    -   **FIXME**: should we reduce the target to leave the domain in a
        neutral state instead of asking it to allocate and fail forever?

Example operation {#example}
=================

![The diagram shows an initial system state comprising 3 domains on a
single host. The state is not ideal; the domains each have the same
policy settings (`dynamic-min` and `dynamic-max`) and yet are using
differing values of $\texttt{totpages'}$. In addition the host has more
memory free than desired. The second diagram shows the result of
computing ideal target values and the third diagram shows the result
after targets have been set and the balloon drivers have
responded.](fig/calculation)

[calculation]

The scenario in Figure [calculation] includes 3 domains (domain 1,
domain 2, domain 3) on a host. Each of the domains has a non-ideal
$\texttt{totpages'}$ value.

Recall we also have the policy constraint that:
$$\texttt{dynamic-min} <= \texttt{target} <= \texttt{dynamic-max}$$
Hypothetically if we reduce `target` by
$\texttt{target}-\texttt{dynamic-min}$ (i.e. by setting
$\texttt{target}\leftarrow\texttt{dynamic-min}$) then we should reduce
`totpages` by the same amount, freeing this much memory on the host. In
the upper-most graph in Figure [calculation] the total amount of memory
which would be freed if we set each of the 3 domain’s
$\texttt{target}\leftarrow\texttt{dynamic-min}$ is:
$$\mathit{d1} + \mathit{d2} + \mathit{d3}$$ In this hypothetical
situation we would now have
$x + s + \mathit{d1} + \mathit{d2} + \mathit{d3}$ free on the host where
$s$ is the host slush fund and $x$ is completely unallocated. Since we
always want to keep the host free memory above $s$, we are free to
return $x + \mathit{d1} + \mathit{d2} + \mathit{d3}$ to guests. If we
use the default built-in proportional policy then, since all domains
have the same `dynamic-min` and `dynamic-max`, each gets the same
fraction of this free memory which we call $g$:
$$g {\stackrel{def}{=}}\frac{x + \mathit{d1} + \mathit{d2} + \mathit{d3}}{3}$$
For each domain, the ideal balloon target is now
$\texttt{target} = \texttt{dynamic-min} + g$. The <span><span
style="font-variant:small-caps;">squeezed</span></span> daemon sets
these targets in two phases, as described in Section [twophase section]

The structure of the daemon {#structure}
===========================

The <span><span style="font-variant:small-caps;">squeezed</span></span>
daemon is a single-threaded daemon which is started by an `init.d`
script. It sits waiting for incoming requests on its toolstack interface
and checks every 10s whether all domain targets are set to the ideal
values (see Section [Ballooning policy]). If an allocation request
arrives or if the domain targets require adjusting then it calls into
the module
[ocaml/xenops/squeeze\_xen.ml](ocaml/xenops/squeeze_xen.ml)[^6].

The module [ocaml/xenops/squeeze\_xen.ml](ocaml/xenops/squeeze_xen.ml)
contains code which inspects the state of the host (through hypercalls
and reading xenstore) and creates a set of records describing the
current state of the host and all the domains. Note this snapshot of
state is not atomic – it is pieced together from multiple hypercalls and
xenstore reads – we assume that the errors generated are small and we
ignore them. These records are passed into the
[ocaml/xenops/squeeze.ml](ocaml/xenops/squeeze.ml)[^7] module where they
are processed and converted into a list of <span>*actions*</span> i.e.
(i) updates to `memory/target` and; (ii) declarations that particular
domains have become <span>*inactive*</span> or <span>*active*</span>.
The rationale for separating the <span><span
style="font-variant:small-caps;">Xen</span></span> interface from the
main ballooning logic was to make testing easier: the module
[ocaml/xenops/squeeze\_test.ml](ocaml/xenops/squeeze_test.ml)[^8]
contains a simple simulator which allows various edge-cases to be
checked.

Issues
======

-   If a linux domU kernel has the netback, blkback or blktap modules
    then they away pages via `alloc_empty_pages_and_pagevec()` during
    boot. This interacts with the balloon driver to break the assumption
    that, reducing the target by $x$ from a neutral value should free
    $x$ amount of memory.

-   Polling the state of the host (particular the xenstore contents) is
    a bit inefficient. Perhaps we should move the policy values
    `dynamic_min` and `dynamic_max` to a separate place in the xenstore
    tree and use watches instead.

-   The memory values given to the domain builder are in units of MiB.
    We may wish to similarly quantise the `target` value or check that
    the `memory-offset` calculation still works.

-   The <span><span style="font-variant:small-caps;">Xen</span></span>
    patch queue reintroduces the lowmem emergency pool[^9]. This was an
    attempt to prevent guests from allocating lowmem before we switched
    to a two-phase target setting procedure. This patch can probably be
    removed.

-   It seems unnecessarily evil to modify an <span>*inactive*</span>
    domain’s `maxmem` leaving $\texttt{maxmem}<\texttt{target}$, causing
    the guest to attempt allocations forwever. It’s probably neater to
    move the `target` at the same time.

-   Declaring a domain <span>*active*</span> just because it makes small
    amounts of progress shouldn’t be enough. Otherwise a domain could
    free 1 byte (or maybe 1 page) every 5s.

-   Likewise, declaring a domain “uncooperative” only if it has been
    <span>*inactive*</span> for 20s means that a domain could alternate
    between <span>*inactive*</span> for 19s and <span>*active*</span>
    for 1s and not be declared “uncooperative”.

[^1]: <http://wiki.xensource.com/xenwiki/Open_Topics_For_Discussion?action=AttachFile&do=get&target=Memory+Overcommit.pdf>

[^2]: <http://xenbits.xen.org/xapi/xen-3.4.pq.hg?file/c01d38e7092a/max-pages-below-tot-pages>

[^3]: The `control/feature-balloon` key is probably the wrong signal.

[^4]: `change_host_free_memory` in
    <http://xenbits.xen.org/xapi/xen-api.hg?file/3e8c0167940d/ocaml/xenops/squeeze.ml>

[^5]: `one_iteration` in
    <http://xenbits.xen.org/xapi/xen-api.hg?file/3e8c0167940d/ocaml/xenops/squeeze.ml>

[^6]: <http://www.xen.org/files/XenCloud/ocamldoc/index.html?c=xenops&m=Squeeze_xen>

[^7]: <http://www.xen.org/files/XenCloud/ocamldoc/index.html?c=xenops&m=Squeeze>

[^8]: <http://www.xen.org/files/XenCloud/ocamldoc/index.html?c=xenops&m=Squeeze_test>

[^9]: <http://xenbits.xen.org/xapi/xen-3.4.pq.hg?file/c01d38e7092a/lowmem-emergency-pool>
