---
title: Schedule Snapshot Design
layout: default
design_doc: true
design_review: 186
revision: 2
status: proposed
revision_history:
- revision_number: 1
  description: Initial version
- revision_number: 2
  description: Renaming VMSS fields and APIs. API message_create superseeds vmss_create_alerts.
- revision_number: 3
  description: Remove VMSS alarm_config details and use existing pool wide alarm config
- revision_number: 4
  description: Renaming field from retention-value to retained-snapshots and schedule-snapshot to scheduled-snapshot
- revision_number: 5
  description: Add new API task_set_status
---

The scheduled snapshot feature will utilize the existing architecture of VMPR. In terms of functionality, scheduled snapshot is basically VMPR without its archiving capability.

Introduction
------------

* Schedule snapshot will be a new object in xapi as VMSS.
* A pool can have multiple VMSS.
* Multiple VMs can be a part of VMSS but a VM cannot be a part of multiple VMSS.
* A VMSS takes VMs snapshot with type [`snapshot`, `checkpoint`, `snapshot_with_quiesce`].
* VMSS takes snapshot of VMs on configured intervals:
	* `hourly` -> On everyday, Each hour, Mins [0;15;30;45]
	* `daily` -> On everyday, Hour [0 to 23], Mins [0;15;30;45]
	* `weekly` -> Days [`Monday`,`Tuesday`,`Wednesday`,`Thursday`,`Friday`,`Saturday`,`Sunday`], Hour[0 to 23], Mins [0;15;30;45]
* VMSS will have a limit on retaining number of VM snapshots in range [1 to 10].

Datapath Design
---------------

* There will be a cron job for VMSS.
* VMSS plugin will go through all the scheduled snapshot policies in the pool and check if any of them are due.
* If a snapshot is due then : Go through all the VM objects in XAPI associated with this scheduled snapshot policy and create a new snapshot.
* If the snapshot operation fails, create a notification alert for the event and move to the next VM.
* Check if an older snapshot now needs to be deleted to comply with the retained snapshots defined in the scheduled policy.
* If we need to delete any existing snapshots, delete the oldest snapshot created via scheduled policy.
* Set the last-run timestamp in the scheduled policy.

Xapi Changes
------------

There is a new record for VM Scheduled Snapshot with new fields.

New fields:

* `name-label` type `String` : Name label for VMSS.
* `name-description` type `String` : Name description for VMSS.
* `enabled` type `Bool` : Enable/Disable VMSS to take snapshot.
* `type` type `Enum` [`snapshot`; `checkpoint`; `snapshot_with_quiesce`] : Type of snapshot VMSS takes.
* `retained-snapshots` type `Int64` : Number of snapshots limit for a VM, max limit is 10 and default is 7.
* `frequency` type `Enum` [`hourly`; `daily`; `weekly`] : Frequency of taking snapshot of VMs.
* `schedule` type `Map(String,String)` with (key, value) pair:
	* hour : 0 to 23
	* min : [0;15;30;45]
	* days : [`Monday`,`Tuesday`,`Wednesday`,`Thursday`,`Friday`,`Saturday`,`Sunday`]
* `last-run-time` type Date : DateTime of last execution of VMSS.
* `VMs` type VM refs : List of VMs part of VMSS.

New fields to VM record:

* `scheduled-snapshot` type VMSS ref : VM part of VMSS.
* `is-vmss-snapshot` type Bool : If snapshot created from VMSS.

New APIs
--------

* vmss_snapshot_now (Ref vmss, Pool_Operater) -> String : This call executes the scheduled snapshot immediately.
* vmss_set_retained_snapshots (Ref vmss, Int value, Pool_Operater) -> unit : Set the value of vmss retained snapshots, max is 10.
* vmss_set_frequency (Ref vmss, String "value", Pool_Operater) -> unit : Set the value of the vmss frequency field.
* vmss_set_type (Ref vmss, String "value", Pool_Operater) -> unit : Set the snapshot type of the vmss type field.
* vmss_set_scheduled (Ref vmss, Map(String,String) "value", Pool_Operater) -> unit : Set the vmss scheduled to take snapshot.
* vmss_add_to_schedule (Ref vmss, String "key", String "value", Pool_Operater) -> unit : Add key value pair to VMSS schedule.
* vmss_remove_from_schedule (Ref vmss, String "key", Pool_Operater) -> unit : Remove key from VMSS schedule.
* vmss_set_last_run_time (Ref vmss, DateTime "value", Local_Root) -> unit : Set the last run time for VMSS.
* task_set_status (Ref task, status_type "value", READ_ONLY) -> unit : Set the status of task owned by same user, Pool_Operator can set status for any tasks.

New CLIs
--------

* vmss-create (required : "name-label";"type";"frequency", optional : "name-description";"enabled";"schedule:";"retained-snapshots") -> unit : Creates VM scheduled snapshot.
* vmss-destroy (required : uuid) -> unit : Destroys a VM scheduled snapshot.
