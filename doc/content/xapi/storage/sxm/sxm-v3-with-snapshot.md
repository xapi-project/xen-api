---
title: SMAPIv3 snapshot-aware storage migration
layout: default
design_doc: true
revision: 1
status: draft
---

<!--toc:start-->
- [Overview](#overview)
- [Why current SMAPIv3 migration fails with snapshots](#why-current-smapiv3-migration-fails-with-snapshots)
  - [The snapshot chain problem](#the-snapshot-chain-problem)
  - [Loss of the parent-child chain](#loss-of-the-parent-child-chain)
  - [How SMAPIv1 avoids the problem](#how-smapiv1-avoids-the-problem)
- [New SMAPIv3 migration mechanism](#new-smapiv3-migration-mechanism)
  - [The approach in one paragraph](#the-approach-in-one-paragraph)
  - [A worked example](#a-worked-example)
  - [Walking through the migration](#walking-through-the-migration)
    - [Phase 1 — Discover the snapshot tree](#phase-1--discover-the-snapshot-tree)
    - [Phase 2 — Mirror the tree](#phase-2--mirror-the-tree)
    - [Phase 3 — Mirror the live leaf](#phase-3--mirror-the-live-leaf)
    - [Phase 4 — Restore metadata](#phase-4--restore-metadata)
  - [Notes](#notes)
<!--toc:end-->

## Overview

This document is a high-level design for adding snapshot-chain awareness to
SMAPIv3 storage migration (SXM). It explains why the current SMAPIv3 SXM
mechanism, which mirrors only the writable leaf VDI of a running VM, cannot
preserve VMs that have snapshots, and proposes a snapshot-aware migration
flow that re-uses the existing NBD-backed QEMU mirror once per node of the
snapshot tree.

The reader is assumed to be familiar with the high-level shape of SXM
already documented in [Storage migration](https://github.com/xapi-project/xen-api/blob/master/doc/content/xapi/storage/sxm/index.md).
That document describes the per-VDI mirror as the unit of work; the
design below extends that unit from a single VDI to a complete snapshot
tree.

## Why current SMAPIv3 migration fails with snapshots

### The snapshot chain problem

When a VM has snapshots, its storage is not a single VDI but a chain of
related VDIs. Each snapshot captures the state of the disk at a particular
point in time, and in current SMAPIv1 and SMAPIv3 backends these snapshots
are realised as a hierarchical structure of differencing nodes from oldest
(base) to newest (leaf).

A typical snapshot scenario looks like:

```
Source SR storage structure (nested parent-child):

                V1 (original base)
               /  \
              V2   S1 (snapshot, taken at T2)
             /  \
    (leaf) V3    S2 (snapshot, taken at T1)

Snapshot chain (XAPI's view):
      S1 (snapshot of V3) → S2 (snapshot of V3) → V3 (leaf)
```

In this structure:

* `V3` is the current leaf VDI (writable, receiving new writes from the
  running VM). It is a child of `V2`, which is a child of `V1` (the
  original base). Snapshots `S1` and `S2` branch off from intermediate
  points in this chain. The parent-child structure exists in the storage
  backend but is not visible to XAPI.
* XAPI sees the leaf and its snapshots, but not the hidden parent nodes
  `V2` and `V1`.
* To read a full, bootable disk image the backend must traverse from leaf
  to base: `V3` → `V2` → `V1`. The snapshots branch off at intermediate
  points.

The nested storage structure represents the real on-disk parent-child
relationships in the backend; the snapshot chain is XAPI's user-facing
view of the same data. The key challenge during migration is that the
backend's chain (`V3` → `V2` → `V1`) must be reproduced on the destination
for snapshots to remain functional after migration.

### Loss of the parent-child chain

The current SMAPIv3 migration mechanism was designed for live VM migration
of a single writable VDI. When a VM with snapshots is migrated, XAPI
transfers the visible VDIs (leaf and snapshots) but the underlying
parent-child relationships between them are not preserved:

```
Current SMAPIv3 migration:

Source SR                           Destination SR
---------                           --------------

      V1                                        V1'
     /  \                                         \
    V2   S1 ------copy----→                V2'    S1' (copied)
   /  \                                       \
 V3   S2  ------copy----→               V3'    S2' (copied)
  │
  └──mirror  ----------→

What gets transferred:
  V3 (leaf)     ----mirrored----→  V3'
  S1, S2        ----copied------→  S1', S2'

What is LOST:
  chain V3' → V2'   (not transferred)
  chain V2' → V1'   (not transferred)
```

Although `V3`, `V2` and `V1` physically exist on the destination as
separate VDIs, the chain between them is not re-created,
The root cause is simple: VDI.similar_content exists in SMAPIv1, but it is not implemented in [SMAPIv3](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi-storage-script/main.ml#L1747) and cannot be, so there is no way to rebuild the chain.

SMAPIv1 rebuilds the chain with a BFS over the snapshot tree. At each node it reads VDI.sm_config.vhd-parent to find that VDI's parent on the destination. This field comes from the
VHD backend, and the whole similar_content / content_id matching depends on it. SMAPIv3 uses qcow2 instead of VHD, so there is no vhd-parent to read, and the chain cannot be rebuilt
the same way. Adding a vhd-parent field for SMAPIv3 is also a bad fix: it would push a storage-internal detail up into XAPI, which is something the storage layer is meant to hide. So
the similar_content + content_id + vhd-parent path does not work for SMAPIv3, and it also breaks the layering. That is why we mirror the tree directly instead.

so the destination ends up with several disconnected trees. The mirrored leaf `V3'` is only
the delta that sat on top of `V2` on the source; without the chain to
`V2'` and `V1'` it has no base data. The snapshot copies `S1'` and `S2'`
likewise have no parent to reference.

The user-visible symptoms are unpleasant: migration appears to succeed
with no error, but when the VM is started on the destination the data
path cannot fetch the full disk (only the leaf delta is reachable) and
the VM fails to boot. Because the chain is lost rather than corrupted,
there is no straightforward rollback.

## New SMAPIv3 migration mechanism

### The approach in one paragraph

The fix is straightforward in spirit: **mirror the entire snapshot tree,
not just the leaf**. We keep the existing NBD-backed QEMU mirror — it
already works well for a single VDI — and apply it once per snapshot,
walking the snapshot tree depth-first. After mirroring each snapshot's
data into a destination VDI we ask the destination SR to take a snapshot
of that VDI, which anchors the data as a real parent on the destination
chain. The mirrored leaf at the end naturally inherits this chain as its
base, so the destination ends up with exactly the same parent-child
topology as the source. A small table of `(source snapshot VDI,
destination snapshot VDI, snapshot time)` tuples is propagated at the end
so XAPI's database and the backend's bookkeeping both reflect the new
layout.

### A worked example

We will use one running example throughout the rest of this section. A
user takes two snapshots, then **reverts** to the first one and takes two
more:

```
Action timeline                      Resulting snapshot tree

  T1: take snap1                            snap1
  T2: take snap2                           /     \
  T3: revert to snap1                  snap2    snap3
  T4: take snap3                                  |
  T5: take snap4                                snap4
  T6: VM keeps running                            |
                                                live
```

Two things to notice:

* The tree has a **branch** at `snap1`. `snap2` belongs to the original
  line that was abandoned by the revert; `snap3 → snap4 → live` is the
  active line the running VM is on today.
* XAPI models this branching via each snapshot VM's `parent` / `children`
  pointers. Walking `VM.parent` upward from the live VM gives the active
  path (`snap4 → snap3 → snap1`); every snapshot VM not on that walk is
  on a reverted side branch.

The source storage backend reproduces the same shape one level down, as a
tree of VDIs with `snap1`'s VDI as the shared base for both branches. The
goal of migration is to reproduce *that* tree on the destination.

### Walking through the migration

Migration runs in four phases. We describe each in terms of the example
above.

#### Phase 1 — Discover the snapshot tree

We need to know which destination VDIs to create and how they should be
related. Starting from the live leaf VDI being migrated, we collect every
VDI in the SR whose `snapshot_of` field points back at that leaf — these
are exactly the snapshot VDIs taken from this disk over time. Each such
snapshot VDI is attached to a snapshot VM, and the `parent` / `children`
pointers among those snapshot VMs give us the tree shape. A single walk
of `VM.parent` upward from the live VM marks which nodes are on the
active path.

Each tree node is a small record:

```ocaml
type snapshot_tree_node = {
    vdi_uuid: string
  ; snapshot_time: Clock.Date.t
  ; on_active_path: bool
  ; children: snapshot_tree_node list
}
```

For our example, the discovery result is:

```
snap1   (active path)
├── snap2   (inactive — orphaned by the revert)
└── snap3   (active path)
    └── snap4   (active path, immediate parent of the live VM)
```

#### Phase 2 — Mirror the tree

We walk the tree depth-first, carrying along a *working VDI* on the
destination. The working VDI starts as `mirror_vdi`, the destination VDI
returned by `receive_start3` for the live leaf. At each node we do the
same two things:

1. Run a one-shot QEMU mirror from the node's source snapshot VDI into
   the working VDI.
2. Ask the destination SR to take a snapshot of the working VDI. The new
   destination snapshot serves two purposes at once: it records the
   mirrored data as a real VDI on the destination chain, and it acts as
   a stable anchor that we can clone if this node turns out to be a
   branch point.

When a node has more than one child (a branch point, caused by a revert),
we **recurse into the inactive subtrees first, on a clone of the anchor**,
and into the active continuation **last, on the same working VDI**. The
clone is torn down as soon as its subtree finishes. This way the working
VDI never leaves the active path, so when the recursion bottoms out it is
still `mirror_vdi`, sitting on top of the deepest active-path snapshot —
ready for the live mirror to take over. No "splice" or rename step is
required.

The DFS body, with most plumbing elided, looks like:

```ocaml
let rec dfs_process_node ~ctx ~working_vdi node =
  let dest_snapshot, relation =
    mirror_node_into ~ctx ~working_vdi node
  in
  let inactive, active =
    List.partition (fun c -> not c.on_active_path) node.children
  in
  let inactive_relations =
    List.concat_map (fun child ->
      let branch_vdi, cleanup = prepare_branch_vdi ~ctx ~dest_snapshot in
      let rels =
        try dfs_process_node ~ctx ~working_vdi:branch_vdi child
        with e -> (try cleanup () with _ -> ()) ; raise e
      in
      cleanup () ; rels
    ) inactive
  in
  let active_relations =
    List.concat_map
      (fun child -> dfs_process_node ~ctx ~working_vdi child)
      active
  in
  relation :: (inactive_relations @ active_relations)
```

Applied to our example, with `W` denoting the working VDI:

| Step | Source  | Operation                              | W after step  |
| ---: | ------- | -------------------------------------- | ------------- |
| 1    | `snap1` | mirror → `W`; snapshot → `D1`          | `mirror_vdi`  |
| 2    | —       | clone `D1`, attach                     | `branch_vdi`  |
| 3    | `snap2` | mirror → `W`; snapshot → `D2`          | `branch_vdi`  |
| 4    | —       | destroy `branch_vdi`                   | `mirror_vdi`  |
| 5    | `snap3` | mirror → `W`; snapshot → `D3`          | `mirror_vdi`  |
| 6    | `snap4` | mirror → `W`; snapshot → `D4`          | `mirror_vdi`  |

At the end the destination has the chain `(D1 → D3 → D4) ← mirror_vdi`
plus `D2` hanging off `D1` — the exact shape of the source tree. We also
keep a table of `(source snapshot VDI, destination snapshot VDI, snapshot
time)` tuples that Phase 4 will need:

```ocaml
type snapshot_relation = {
    src_vdi: Storage_interface.Vdi.t
  ; dest_vdi: Storage_interface.Vdi.t
  ; snapshot_time: Clock.Date.t
}
```

Timestamps are kept as `Clock.Date.t` throughout, and only serialised to
RFC3339 at the boundary where they are handed to another component
(e.g. the `update_snapshot_info` RPC).

These relations are stashed under the mirror ID in a small
mutex-protected table in `Storage_migrate_helper.State` so the
VM-migration orchestrator can pick them up once the live mirror is
complete.

#### Phase 3 — Mirror the live leaf

This is the original SMAPIv3 flow, with one small wrapper. The snapshots
in Phase 2 were taken while `mirror_vdi` was attached read-only on the
destination (snapshotting an actively-written VDI is not safe); we now
flip it back to writable and run the continuous QEMU mirror from the
running VM's leaf into `mirror_vdi`, exactly as before. Because Phase 2
left `mirror_vdi` positioned on top of the correct chain, the live mirror
lands on a complete base — no separate splicing step is required.

#### Phase 4 — Restore metadata

Snapshot VDIs now exist on the destination, but the destination SR does
not yet know they are snapshots. We reuse the pre-existing SMAPIv1 SXM
path for this step: the orchestrator in `xapi_vm_migrate.ml` feeds the
per-snapshot mirror records into the same `update_snapshot_info` flow
that legacy migration already uses, which RPCs into
`SR.update_snapshot_info_dest` on the destination. For each entry that
RPC sets `snapshot_of`, `snapshot_time` and `is_a_snapshot` in the XAPI
database and pushes the same fields into the backend's own custom-key
store (so the storage backend's view stays in sync across an `SR.scan`).
The `content_id` check it performs is satisfied because Phase 2
propagated `content_id` per snapshot during the DFS. Finally the
orchestrator updates each snapshot VM's VBD to reference the destination
snapshot VDI.

The destination now has both the data and the topology, in both XAPI and
the storage backend, and the snapshot tree is fully usable: snapshots can
be inspected, booted, and reverted to.

After the migration is finished, it should ensure:
- On migration failure, the destination must be left clean (no VDIs present)
- On successful migration, all source VDIs must be removed
- On successful migration, the VDI parent-child relationships must be preserved at the destination

### Notes

Two non-obvious choices are worth highlighting:

* **Disk identity by `snapshot_of`.** Every snapshot VDI carries a
  `snapshot_of` reference back to the live leaf VDI it was taken from.
  Collecting all VDIs whose `snapshot_of` equals the leaf being migrated
  yields exactly the set of snapshot VDIs for that disk, directly at the
  storage layer, without having to walk VMs and VBDs. Reverts do not
  invalidate this: when XAPI reverts to a snapshot it re-points the
  remaining snapshots' `snapshot_of` at the new live leaf, so the field
  stays consistent with the current leaf at all times.
* **DFS, inactive-first, active-last.** Processing inactive subtrees
  before the active continuation is what lets us reuse `mirror_vdi` as
  the working VDI for the whole active path: each inactive branch comes
  and goes on a short-lived clone, and `mirror_vdi` is never displaced.
  It also bounds the number of simultaneously-attached destination VDIs
  to `O(tree depth)` rather than `O(branch count)`.
