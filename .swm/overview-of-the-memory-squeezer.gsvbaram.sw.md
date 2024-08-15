---
hidden: true
title: Overview of the memory squeezer
---
{{% notice warning %}} This was converted to markdown from squeezer.tex. It is not clear how much of this document is still relevant and/or already present in the other docs. {{% /notice %}}

## summary

- ballooning is a per-domain operation; not a per-VM operation. A VM may be represented by multiple domains (currently localhost migrate, in the future stubdomains)

- most free host memory is divided up between running domains proportionally, so they all end up with the same value of <span>ratio</span>

<!-- -->

```
  where ratio(domain) = 
     if domain.dynamic_max - domain.dynamic_min = 0 
     then 0 
     else (domain.target - domain.dynamic_min) 
          / (domain.dynamic_max - domain.dynamic_min)
```

## Assumptions

- all memory values are stored and processed in units of KiB

- the squeezing algorithm doesn’t know about host or VM overheads but this doesn’t matter because

- the squeezer assumes that any free host memory can be allocated to running domains and this will be directly reflected in their memory_actual i.e. if x KiB is free on the host we can tell a guest to use x KiB and see the host memory goes to 0 and the guest’s memory_actual increase by x KiB. We assume that no-extra ’overhead’ is required in this operation (all overheads are functions of static_max only)

## Definitions

- domain: an object representing a xen domain

- domain.domid: unique identifier of the domain on the host

- domaininfo(domain): a function which returns live per-domain information from xen (in real-life a hypercall)

- a domain is said to “have never run” if never_been_run(domain)

  ```
    where never_been_run(domain) = domaininfo(domain).paused 
        and not domaininfo(domain).shutdown 
        and domaininfo(domain).cpu_time = 0
  ```

- xenstore-read(path): a function which returns the value associated with ’path’ in xenstore

- domain.initial_reservation: used to associate freshly freed memory with a new domain which is being built or restored

  ```
    domain.initial_reservation = 
      xenstore-read(/local/domain/<domain.domid>/memory/initial-reservation)
  ```

- domain.target: represents what we think the balloon target currently is

  ```
    domain.target = 
        if never_been_run(domain)
        then xenstore-read(/local/domain/<domain.domid>/memory/target)
        else domain.initial_reservation
  ```

- domain.dynamic_min: represents what we think the dynamic_min currently is

  ```
    domain.dynamic_min = 
        if never_been_run(domain)
        then xenstore-read(/local/domain/<domain.domid>/memory/dynamic_min)
        else domain.initial_reservation
  ```

- domain.dynamic_max: represents what we think the dynamic_max currently is

  ```
    domain.dynamic_max = 
        if never_been_run(domain)
        then xenstore-read(/local/domain/<domain.domid>/memory/dynamic_max)
        else domain.initial_reservation
  ```

- domain.memory_actual: represents the memory we think the guest is using (doesn’t take overheads like shadow into account)

  ```
    domain.memory_actual = 
        if never_been_run(domain)
        max domaininfo(domain).total_memory_pages domain.initial_reservation
        else domaininfo(domain).total_memory_pages
  ```

- domain.memory_actual_last_update_time: time when we saw the last change in memory_actual

- domain.unaccounted_for: a fresh domain has memory reserved for it but xen doesn’t know about it. We subtract this from the host memory xen thinks is free.

  ```
    domain.unaccounted_for =
        if never_been_run(domain)
        then max 0 (domain.initial_reservation - domaininfo(domain).total_memory_pages)
  ```

- domain.max_mem: an upper-limit on the amount of memory a domain can allocate. Initially static_max.

  ```
    domain.max_mem = domaininfo(domain).max_mem
  ```

- assume_balloon_driver_stuck_after: a constant number of seconds after which we conclude that the balloon driver has stopped working

  ```
    assume_balloon_driver_stuck_after = 2
  ```

- domain.active: a boolean value which is true when we think the balloon driver is functioning

  ```
    domain.active = has_hit_target(domain) 
        or (now - domain.memory_actual_last_update_time) 
             > assume_balloon_driver_stuck_after
  ```

- a domain is said to “have hit its target” if has_hit_target(domain)

  ```
    where has_hit_target(domain) = floor(memory_actual / 4) = floor(target / 4)
  ```

  NB this definition might have to be loosened if it turns out that some drivers are less accurate than this.

- a domain is said to “be capable of ballooning” if can_balloon(domain) where can_balloon(domain) = not domaininfo(domain).paused

- host: an object representing a XenServer host

- host.domains: a list of domains present on the host

- physinfo(host): a function which returns live per-host information from xen (in real-life a hypercall)

- host.free_mem: amount of memory we consider to be free on the host

  ```
    host.free_mem = physinfo(host).free_pages + physinfo(host).scrub_pages 
      - \sigma d\in host.domains. d.unaccounted_for
  ```

## Squeezer APIs

The squeezer has 2 APIs:

1. allocate-memory-for-domain(host, domain, amount): frees “amount” and “reserves” (as best it can) it for a particular domain

2. rebalance-memory: called after e.g. domain destruction to rebalance memory between the running domains

allocate-memory-for-domain keeps contains the main loop which performs the actual target and max_mem adjustments:

```
function allocate-memory-for-domain(host, domain, amount):
  \forall d\in host.domains. d.max_mem <- d.target
  while true do
    -- call change-host-free-memory with a "success condition" set to 
    -- "when the host memory is >= amount"
    declared_active, declared_inactive, result = 
        change-host-free-memory(host, amount, \lambda m >= amount)
    if result == Success:
      domain.initial_reservation <- amount
      return Success
    elif result == DynamicMinsTooHigh:
      return DynamicMinsTooHigh
    elif result == DomainsRefusedToCooperate:
      return DomainsRefusedToCooperate
    elif result == AdjustTargets(adjustments):
      \forall (domain, target)\in adjustments:
         domain.max_mem <- target
         domain.target <- target

    \forall d\in declared_inactive:
      domain.max_mem <- min domain.target domain.memory_actual
    \forall d\in declared_active:
      domain.max_mem <- domain.target
  done
```

The helper function change-host-free-memory(host, amount) does the “thinking”:

1. it keeps track of whether domains are active or inactive (only for the duration of the squeezer API call – when the next call comes in we assume that all domains are active and capable of ballooning... a kind of “innocent until proven guilty” approaxh)

2. it computes what the balloon targets should be

<!-- -->

```
function change-host-free-memory(host, amount, success_condition):
  \forall d\in host.domains. recalculate domain.active
  active_domains <- d\in host.domains where d.active = true
  inactive_domains <- d\in host.domains where d.active = false
  -- since the last time we were called compute the lists of domains 
  -- which have become active and inactive
  declared_active, declared_inactive <- ...
  -- compute how much memory we could free or allocate given only the 
  -- active domains
  maximum_freeable_memory = 
     sum(d\in active_domains)(d.memory_actual - d.dynamic_min)
  maximum_allocatable_memory = 
     sum(d\in active_domains)(d.dynamic_max - d.memory_actual)
  -- hypothetically consider freeing the maximum memory possible. 
  -- How much would we have to give back after we've taken as much as we want?
  give_back = max 0 (maximum_freeable_memory - amount)
  -- compute a list of target changes to 'give this memory back' to active_domains
  -- NB this code is careful to allocate *all* memory, not just most 
  -- of it because of a rounding error.
  adjustments = ...
  -- decide whether every VM has reached its target (a good thing)
  all_targets_reached = true if \forall d\in active_domains.has_hit_target(d)
  
  -- If we're happy with the amount of free memory we've got and the active 
  -- guests have finished ballooning
  if success_condition host.free_mem = true 
     and all_targets_reached and adjustments = []
  then return declared_active, declared_inactive, Success
  
  -- If we're happy with the amount of free memory and the running domains 
  -- can't absorb any more of the surplus
  if host.free_mem >= amount and host.free_mem - maximum_allocatable_memory = 0
  then return declared_active, declared_inactive, Success

  -- If the target is too aggressive because of some non-active domains
  if maximum_freeable_memory < amount and inactive_domains <> []
  then return declared_active, declared_inactive, 
           DomainsRefusedToCooperate inactive_domains

  -- If the target is too aggressive not because of the domains themselves 
  -- but because of the dynamic_mins
  return declared_active, declared_inactive, DynamicMinsTooHigh
```

The API rebalance-memory aims to use up as much host memory as possible EXCEPT it is necessary to keep some around for xen to use to create empty domains with.

```
Currently we have:
 -- 10 MiB
 target_host_free_mem = 10204
 -- it's not always possible to allocate everything so a bit of slop has 
 -- been added here:
 free_mem_tolerance = 1024

function rebalance-memory(host):
  change-host-free-memory(host, target_host_free_mem, 
      \lambda m. m - target_host_free_mem < free_mem_tolerance)
  -- and then wait for the xen page scrubber
```

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
