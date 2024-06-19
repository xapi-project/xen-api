---
title: Aggregated Local Storage and Host Reboots
layout: default
design_doc: true
revision: 3
status: proposed
design_review: 144
revision_history:
- revision_number: 1
  description: Initial version
- revision_number: 2
  description: Included some open questions under Xapi point 2
- revision_number: 3
  description: Added new error, task, and assumptions
---

## Introduction

When hosts use an aggregated local storage SR, then disks are going to be mirrored to several different hosts in the pool (RAID). This ensures that if a host goes down (e.g. due to a reboot after installing a hotfix or upgrade, or when "fenced" by the HA feature), all disk contents in the SR are still accessible. This also means that if all disks are mirrored to just two hosts (worst-case scenario), just one host may be down at any point in time to keep the SR fully available.

When a node comes back up after a reboot, it will resynchronise all its disks with the related mirrors on the other hosts in the pool. This syncing takes some time, and only after this is done, we may consider the host "up" again, and allow another host to be shut down.

Therefore, when installing a hotfix to a pool that uses aggregated local storage, or doing a rolling pool upgrade, we need to make sure that we do hosts one-by-one, and we wait for the storage syncing to finish before doing the next.

This design aims to provide guidance and protection around this by blocking hosts to be shut down or rebooted from the XenAPI except when safe, and setting the `host.allowed_operations` field accordingly.


## XenAPI

If an aggregated local storage SR is in use, and one of the hosts is rebooting or down (for whatever reason), or resynchronising its storage, the operations `reboot` and `shutdown` will be removed from the `host.allowed_operations` field of _all_ hosts in the pool that have a PBD for the SR.

This is a conservative approach in that assumes that this kind of SR tolerates only one node "failure", and assumes no knowledge about how the SR distributes its mirrors. We may refine this in future, in order to allow some hosts to be down simultaneously.

The presence of the `reboot` operation in `host.allowed_operations` indicates whether the `host.reboot` XenAPI call is allowed or not (similarly for `shutdown` and `host.shutdown`). It will not, of course, prevent anyone from rebooting a host from the dom0 console or power switch.

Clients, such as XenCenter, can use `host.allowed_operations`, when applying an update to a pool, to guide them when it is safe to update and reboot the next host in the sequence.

In case `host.reboot` or `host.shutdown` is called while the storage is busy resyncing mirrors, the call will fail with a new error `MIRROR_REBUILD_IN_PROGRESS`.

## Xapi

Xapi needs to be able to:

1. Determine whether aggregated local storage is in use; this just means that a PBD for such an SR present.
	* TBD: To avoid SR-specific code in xapi, the storage backend should tell us whether it is an aggregated local storage SR.
2. Determine whether the storage system is resynchronising its mirrors; it will need to be able to query the storage backend for this kind of information.
	* Xapi will poll for this and will reflect that a resync is happening by creating a `Task` for it (in the DB). This task can be used to track progress, if available.
	* The exact way to get the syncing information from the storage backend is SR specific. The check may be implemented in a separate script or binary that xapi calls from the polling thread. Ideally this would be integrated with the storage backend.
3. Update `host.allowed_operations` for all hosts in the pool according to the rules described above. This comes down to updating the function `valid_operations` in `xapi_host_helpers.ml`, and will need to use a combination of the functionality from the two points above, plus and indication of host liveness from `host_metrics.live`.
4. Trigger an update of the allowed operations when a host shuts down or reboots (due to a XenAPI call or otherwise), and when it has finished resynchronising when back up. Triggers must be in the following places (some may already be present, but are listed for completeness, and to confirm this):
	* Wherever `host_metrics.live` is updated to detect pool slaves going up and down (probably at least in `Db_gc.check_host_liveness` and `Xapi_ha`).
	* Immediately when a `host.reboot` or `host.shutdown` call is executed: `Message_forwarding.Host.{reboot,shutdown,with_host_operation}`.
	* When a storage resync is starting or finishing.

All of the above runs on the pool master (= SR master) only.

## Assumptions

The above will be safe if the storage cluster is equal to the XenServer pool. In general, however, it may be desirable to have a storage cluster that is larger than the pool, have multiple XS pools on a single cluster, or even share the cluster with other kinds of nodes.

To ensure that the storage is "safe" in these scenarios, xapi needs to be able to ask the storage backend:

1. if a mirror is being rebuilt "somewhere" in the cluster, AND
2. if "some node" in the cluster is offline (even if the node is not in the XS pool).

If the cluster is equal to the pool, then xapi can do point 2 without asking the storage backend, which will simplify things. For the moment, we assume that the storage cluster is equal to the XS pool, to avoid making things too complicated (while still need to keep in mind that we may change this in future).

