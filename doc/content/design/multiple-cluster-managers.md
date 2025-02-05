---
title: Multiple Cluster Managers
layout: default
design_doc: true
revision: 2
status: confirmed
revision_history:
- revision_number: 1
  description: Initial revision
- revision_number: 2
  description: Short-term simplications and scope reduction
---

Introduction
------------

Xapi currently uses a cluster manager called [xhad](../toolstack/features/HA/index). Sometimes other software comes with its own built-in way of managing clusters, which would clash with xhad (example: xhad could choose to fence node 'a' while the other system could fence node 'b' resulting in a total failure). To integrate xapi with this other software we have 2 choices:

1. modify the other software to take membership information from xapi; or
2. modify xapi to take membership information from this other software.

This document proposes a way to do the latter.

XenAPI changes
--------------

### New field

We will add the following new field:

- `pool.ha_cluster_stack` of type `string` (read-only)
  - If HA is enabled, this field reflects which cluster stack is in use.
  - Set to `"xhad"` on upgrade, which implies that so far we have used XenServer's own cluster stack, called `xhad`.

### Cluster-stack choice

We assume for now that a particular cluster manager will be mandated (only) by certain types of clustered storage, recognisable by SR type (e.g. OCFS2 or Melio). The SR backend will be able to inform xapi if the SR needs a particular cluster stack, and if so, what is the name of the stack.

When `pool.enable_ha` is called, xapi will determine which cluster stack to use based on the presence or absence of such SRs:

- If an SR that needs its own cluster stack is attached to the pool, then xapi will use that cluster stack.
- If no SR that needs a particular cluster stack is attached to the pool, then xapi will use `xhad`.

If multiple SRs that need a particular cluster stack exist, then the storage parts of xapi must ensure that no two such SRs are ever attached to a pool at the same time.

### New errors

We will add the following API error that may be raised by `pool.enable_ha`:

- `INCOMPATIBLE_STATEFILE_SR`: the specified SRs (`heartbeat_srs` parameter) are not of the right type to hold the HA statefile for the `cluster_stack` that will be used. For example, there is a Melio SR attached to the pool, and therefore the required cluster stack is the Melio one, but the given heartbeat SR is not a Melio SR. The single parameter will be the name of the required SR type.

The following new API error may be raised by `PBD.plug`:

- `INCOMPATIBLE_CLUSTER_STACK_ACTIVE`: the operation cannot be performed because an incompatible cluster stack is active. The single parameter will be the name of the required cluster stack. This could happen (or example) if you tried to create an OCFS2 SR with XenServer HA already enabled.

### Future extensions

In future, we may add a parameter to explicitly choose the cluster stack:

- New parameter to `pool.enable_ha` called `cluster_stack` of type `string` which will have the default value of empty string (meaning: let the implementation choose).
- With the additional parameter, `pool.enable_ha` may raise two new errors:
  - `UNKNOWN_CLUSTER_STACK`:
    The operation cannot be performed because the requested cluster stack does not exist. The user should check the name was entered correctly and, failing that, check to see if the software is installed. The exception will have a single parameter: the name of the cluster stack which was not found.
  - `CLUSTER_STACK_CONSTRAINT`: HA cannot be enabled with the provided cluster stack because some third-party software is already active which requires a different cluster stack setting. The two parameters are: a reference to an object (such as an SR) which has created the restriction, and the name of the cluster stack that this object requires.

Implementation
--------------

The `xapi.conf` file will have a new field: `cluster-stack-root` which will have the default value `/usr/libexec/xapi/cluster-stack`. The existing `xhad` scripts and tools will be moved to `/usr/libexec/xapi/cluster-stack/xhad/`. A hypothetical cluster stack called `foo` would be placed in `/usr/libexec/xapi/cluster-stack/foo/`.

In `Pool.enable_ha` with `cluster_stack="foo"` we will verify that the subdirectory `<cluster-stack-root>/foo` exists. If it does not exist, then the call will fail with `UNKNOWN_CLUSTER_STACK`.

Alternative cluster stacks will need to conform to the exact same interface as [xhad](../toolstack/features/HA).
