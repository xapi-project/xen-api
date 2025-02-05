---
title: CPU feature levelling 2.0
layout: default
design_doc: true
status: released (7.0)
revision: 7
revision_history:
- revision_number: 1
  description: Initial version
- revision_number: 2
  description: Add details about VM migration and import
- revision_number: 3
  description: Included and excluded use cases
- revision_number: 4
  description: Rolling Pool Upgrade use cases
- revision_number: 5
  description: Lots of changes to simplify the design
- revision_number: 6
  description: Use case refresh based on simplified design
- revision_number: 7
  description: RPU refresh based on simplified design
---

Executive Summary
=================

The old XS 5.6-style Heterogeneous Pool feature that is based around hardware-level CPUID masking will be replaced by a safer and more flexible software-based levelling mechanism.

History
=======

- Original XS 5.6 design: [heterogeneous-pools](heterogeneous-pools)
- Changes made in XS 5.6 FP1 for the DR feature (added CPUID checks upon migration)
- XS 6.1: migration checks extended for cross-pool scenario

High-level Interfaces and Behaviour
===================================

A VM can only be migrated safely from one host to another if both hosts offer the set of CPU features which the VM expects.  If this is not the case, CPU features may appear or disappear as the VM is migrated, causing it to crash.   The purpose of feature levelling is to hide features which the hosts do not have in common from the VM, so that it does not see any change in CPU capabilities when it is migrated.

Most pools start off with homogenous hardware, but over time it may become impossible to source new hosts with the same specifications as the ones already in the pool.   The main use of feature levelling is to allow such newer, more capable hosts to be added to an existing pool while preserving the ability to migrate existing VMs to any host in the pool.

Principles for Migration
------------------------

The CPU levelling feature aims to both:

1. Make VM migrations _safe_ by ensuring that a VM will see the same CPU features before and after a migration.
2. Make VMs as _mobile_ as possible, so that it can be freely migrated around in a XenServer pool.

To make migrations safe:

* A migration request will be blocked if the destination host does not offer the some of the CPU features that the VM currently sees.
* Any additional CPU features that the destination host is able to offer will be hidden from the VM.

_Note:_ Due to the limitations of the old Heterogeneous Pools feature, we are not able to guarantee the safety of VMs that are migrated to a Levelling-v2 host from an older host, during a rolling pool upgrade. This is because such VMs may be using CPU features that were not captured in the old feature sets, of which we are therefore unaware. However, migrations between the same two hosts, but before the upgrade, may have already been unsafe. The promise is that we will not make migrations _more_ unsafe during a rolling pool upgrade.

To make VMs mobile:

* A VM that is started in a XenServer pool will be able to see only CPU features that are common to all hosts in the pool. The set of common CPU features is referred to in this document as the _pool CPU feature level_, or simply the _pool level_.

Use Cases for Pools
-------------------

1. A user wants to add a new host to an existing XenServer pool.  The new host has all the features of the existing hosts, plus extra features which the existing hosts do not.  The new host will be allowed to join the pool, but its extra features will be hidden from VMs that are started on the host or migrated to it.  The join does not require any host reboots.

2. A user wants to add a new host to an existing XenServer pool.  The new host does not have all the features of the existing ones.  XenCenter warns the user that adding the host to the pool is possible, but it would lower the pool's CPU feature level.  The user accepts this and continues the join.  The join does not require any host reboots.  VMs that are started anywhere on the pool, from now on, will only see the features of the new host (the lowest common denominator), such that they are migratable to any host in the pool, including the new one.  VMs that were running before the pool join will not be migratable to the new host, because these VMs may be using features that the new host does not have.  However, after a reboot, such VMs will be fully mobile.

3. A user wants to add a new host to an existing XenServer pool.  The new host does not have all the features of the existing ones, and at the same time, it has certain features that the pool does not have (the feature sets overlap).  This is essentially a combination of the two use cases above, where the pool's CPU feature level will be downgraded to the intersection of the feature sets of the pool and the new host.  The join does not require any host reboots.

4. A user wants to upgrade or repair the hardware of a host in an existing XenServer pool.  After upgrade the host has all the features it used to have, plus extra features which other hosts in the pool do not have.   The extra features are masked out and the host resumes its place in the pool when it is booted up again.

5. A user wants to upgrade or repair the hardware of a host in an existing XenServer pool.  After upgrade the host has fewer features than it used to have.  When the host is booted up again, the pool CPU's feature level will be automatically lowered, and the user will be alerted of this fact (through the usual alerting mechanism).

6. A user wants to remove a host from an existing XenServer pool.  The host will be removed as normal after any VMs on it have been migrated away.  The feature set offered by the pool will be automatically re-levelled upwards in case the host which was removed was the least capable in the pool, and additional features common to the remaining hosts will be unmasked.


Rolling Pool Upgrade
--------------------

* A VM which was running on the pool before the upgrade is expected to continue to run afterwards.  However, when the VM is migrated to an upgraded host, some of the CPU features it had been using might disappear, either because they are not offered by the host or because the new feature-levelling mechanism hides them.  To have the best chance for such a VM to successfully migrate (see the note under "Principles for Migration"), it will be given a temporary VM-level feature set providing all of the destination's CPU features that were unknown to XenServer before the upgrade.  When the VM is rebooted it will inherit the pool-level feature set.

* A VM which is started during the upgrade will be given the current pool-level feature set.  The pool-level feature set may drop after the VM is started, as more hosts are upgraded and re-join the pool, however the VM is guaranteed to be able to migrate to any host which has already been upgraded.  If the VM is started on the master, there is a risk that it may only be able to run on that host.

* To allow the VMs with grandfathered-in flags to be migrated around in the pool, the intra pool VM migration pre-checks will compare the VM's feature flags to the target host's flags, not the pool flags.  This will maximise the chance that a VM can be migrated somewhere in a heterogeneous pool, particularly in the case where only a few hosts in the pool do not have features which the VMs require.
  
* To allow cross-pool migration, including to pool of a higher XenServer version, we will still check the VM's requirements against the *pool-level* features of the target pool.  This is to avoid the possibility that we migrate a VM to an 'island' in the other pool, from which it cannot be migrated any further.


XenAPI Changes
--------------

### Fields

* `host.cpu_info` is a field of type `(string -> string) map` that contains information about the CPUs in a host. It contains the following keys: `cpu_count`, `socket_count`, `vendor`, `speed`, `modelname`, `family`, `model`, `stepping`, `flags`, `features`, `features_after_reboot`, `physical_features` and `maskable`.
	* The following keys are specific to hardware-based CPU masking and will be removed: `features_after_reboot`, `physical_features` and `maskable`.
	* The `features` key will continue to hold the current CPU features that the host is able to use. In practise, these features will be available to Xen itself and dom0; guests may only see a subset. The current format is a string of four 32-bit words represented as four groups of 8 hexadecimal digits, separated by dashes. This will change to an arbitrary number of 32-bit words. Each bit at a particular position (starting from the left) still refers to a distinct CPU feature (`1`: feature is present; `0`: feature is absent), and feature strings may be compared between hosts. The old format simply becomes a special (4 word) case of the new format, and bits in the same position may be compared between old and new feature strings.
	* The new key `features_pv` will be added, representing the subset of `features` that the host is able to offer to a PV guest.
	* The new key `features_hvm` will be added, representing the subset of `features` that the host is able to offer to an HVM guest.
* A new field `pool.cpu_info` of type `(string -> string) map` (read only) will be added. It will contain:
	* `vendor`: The common CPU vendor across all hosts in the pool.
	* `features_pv`: The intersection of `features_pv` across all hosts in the pool, representing the feature set that a PV guest will see when started on the pool.
	* `features_hvm`: The intersection of `features_hvm` across all hosts in the pool, representing the feature set that an HVM guest will see when started on the pool.
	* `cpu_count`: the total number of CPU cores in the pool.
	* `socket_count`: the total number of CPU sockets in the pool.
* The `pool.other_config:cpuid_feature_mask` override key will no longer have any effect on pool join or VM migration.
* The field `VM.last_boot_CPU_flags` will be updated to the new format (see `host.cpu_info:features`). It will still contain the feature set that the VM was started with as well as the vendor (under the `features` and `vendor` keys respectively).

### Messages

* `pool.join` currently requires that the CPU vendor and feature set (according to `host.cpu_info:vendor` and `host.cpu_info:features`) of the joining host are equal to those of the pool master. This requirement will be loosened to mandate only equality in CPU vendor:
	* The join will be allowed if `host.cpu_info:vendor` equals `pool.cpu_info:vendor`.
	* This means that xapi will additionally allow hosts that have a _more_ extensive feature set than the pool (as long as the CPU vendor is common). Such hosts are transparently down-levelled to the pool level (without needing reboots).
	* This further means that xapi will additionally allow hosts that have a _less_ extensive feature set than the pool (as long as the CPU vendor is common). In this case, the pool is transparently down-levelled to the new host's level (without needing reboots). Note that this does not affect any running VMs in any way; the mobility of running VMs will not be restricted, which can still migrate to any host they could migrate to before. It does mean that those running VMs will not be migratable to the new host.
	* The current error raised in case of a CPU mismatch is `POOL_HOSTS_NOT_HOMOGENEOUS` with `reason` argument `"CPUs differ"`. This will remain the error that is raised if the pool join fails due to incompatible CPU vendors.
	* The `pool.other_config:cpuid_feature_mask` override key will no longer have any effect.
* `host.set_cpu_features` and `host.reset_cpu_features` will be removed: it is no longer to use the old method of CPU feature masking (CPU feature sets are controlled automatically by xapi). Calls will fail with `MESSAGE_REMOVED`.
* VM lifecycle operations will be updated internally to use the new feature fields, to ensure that:
	* Newly started VMs will be given CPU features according to the pool level for maximal mobility.
	* For safety, running VMs will maintain their feature set across migrations and suspend/resume cycles. CPU features will transparently be hidden from VMs.
	* Furthermore, migrate and resume will only be allowed in case the target host's CPUs are capable enough, i.e. `host.cpu_info:vendor` = `VM.last_boot_CPU_flags:vendor` and `host.cpu_info:features_{pv,hvm}` ⊇ `VM.last_boot_CPU_flags:features`. A `VM_INCOMPATIBLE_WITH_THIS_HOST` error will be returned otherwise (as happens today).
	* For cross pool migrations, to ensure maximal mobility in the target pool, a stricter condition will apply: the VM must satisfy the pool CPU level rather than just the target host's level: `pool.cpu_info:vendor` = `VM.last_boot_CPU_flags:vendor` and `pool.cpu_info:features_{pv,hvm}` ⊇ `VM.last_boot_CPU_flags:features`


CLI Changes
-----------

The following changes to the `xe` CLI will be made:

* `xe host-cpu-info` (as well as `xe host-param-list` and friends) will return the fields of `host.cpu_info` as described above.
* `xe host-set-cpu-features` and `xe host-reset-cpu-features` will be removed.
* `xe host-get-cpu-features` will still return the value of `host.cpu_info:features` for a given host.

Low-level implementation
========================

Xenctrl
-------

The old `xc_get_boot_cpufeatures` hypercall will be removed, and replaced by two new functions, which are available to xenopsd through the Xenctrl module:

	external get_levelling_caps : handle -> int64 = "stub_xc_get_levelling_caps"
	
	type featureset_index = Featureset_host | Featureset_pv | Featureset_hvm
	external get_featureset : handle -> featureset_index -> int64 array = "stub_xc_get_featureset"

In particular, the `get_featureset` function will be used by xapi/xenopsd to ask Xen which are the widest sets of CPU features that it can offer to a VM (PV or HVM). I don't think there is a use for `get_levelling_caps` yet.

Xenopsd
-------

* Update the type `Host.cpu_info`, which contains all the fields that need to go into the `host.cpu_info` field in the xapi DB. The type already exists but is unused. Add the function `HOST.get_cpu_info` to obtain an instance of the type. Some code from xapi and the cpuid.ml from xen-api-libs can be reused.
* Add a platform key `featureset` (`Vm.t.platformdata`), which xenopsd will write to xenstore along with the other platform keys (no code change needed in xenopsd). Xenguest will pick this up when a domain is created, and will apply the CPUID policy to the domain. This has the effect of masking out features that the host may have, but which have a `0` in the feature set bitmap.
* Review current cpuid-related functions in `xc/domain.ml`.

Xapi
----

### Xapi startup

* Update `Create_misc.create_host_cpu` function to use the new xenopsd call. 
* If the host features fall below pool level, e.g. due to a change in hardware: down-level the pool by updating `pool.cpu_info.features_{pv,hvm}`. Newly started VMs will inherit the new level; already running VMs will not be affected, but will not be able to migrate to this host.
* To notify the admin of this event, an API alert (message) will be set: `pool_cpu_features_downgraded`.
	
### VM start

- Inherit feature set from pool (`pool.cpu_info.features_{pv,hvm}`) and set `VM.last_boot_CPU_flags` (`cpuid_helpers.ml`).
- The domain will be started with this CPU feature set enabled, by writing the feature set string to `platformdata` (see above).

### VM migrate and resume

- There are already CPU compatiblity checks on migration, both in-pool and cross-pool, as well as resume. Xapi compares `VM.last_boot_CPU_flags` of the VM to-migrate with `host.cpu_info` of the receiving host. Migration is only allowed if the CPU vendors and the same, and `host.cpu_info:features` ⊇ `VM.last_boot_CPU_flags:features`. The check can be overridden by setting the `force` argument to `true`.
- For in-pool migrations, these checks will be updated to use the appropriate `features_pv` or `features_hvm` field.
- For cross-pool migrations. These checks will be updated to use `pool.cpu_info` (`features_pv` or `features_hvm` depending on how the VM was booted) rather than `host.cpu_info`.
- If the above checks pass, then the `VM.last_boot_CPU_flags` will be maintained, and the new domain will be started with the same CPU feature set enabled, by writing the feature set string to `platformdata` (see above).
- In case the VM is migrated to a host with a higher xapi software version (e.g. a migration from a host that does not have CPU levelling v2), the feature string may be longer. This may happen during a rolling pool upgrade or a cross-pool migration, or when a suspended VM is resume after an upgrade. In this case, the following safety rules apply:
	- Only the existing (shorter) feature string will be used to determine whether the migration will be allowed. This is the best we can do, because we are unaware of the state of the extended feature set on the older host.
	- The existing feature set in `VM.last_boot_CPU_flags` will be extended with the extra bits in `host.cpu_info:features_{pv,hvm}`, i.e. the widest feature set that can possibly be granted to the VM (just in case the VM was using any of these features before the migration).
	- Strictly speaking, a migration of a VM from host A to B that was allowed before B was upgraded, may no longer be allowed after the upgrade, due to stricter feature sets in the new implementation (from the `xc_get_featureset` hypercall). However, the CPU features that are switched off by the new implementation are features that a VM would not have been able to actually use. We therefore need a don't-care feature set (similar to the old `pool.other_config:cpuid_feature_mask` key) with bits that we may ignore in migration checks, and switch off after the migration. This will be a xapi config file option.
	- XXX: Can we actually block a cross-pool migration at the receiver end??

### VM import

The `VM.last_boot_CPU_flags` field must be upgraded to the new format (only really needed for VMs that were suspended while exported; `preserve_power_state=true`), as described above.

### Pool join

Update pool join checks according to the rules above (see `pool.join`), i.e. remove the CPU features constraints.

### Upgrade

* The pool level (`pool.cpu_info`) will be initialised when the pool master upgrades, and automatically adjusted if needed (downwards) when slaves are upgraded, by each upgraded host's started sequence (as above under "Xapi startup").
* The `VM.last_boot_CPU_flags` fields of running and suspended VMs will be "upgraded" to the new format on demand, when a VM is migrated to or resume on an upgraded host, as described above.


XenCenter integration
---------------------

- Don't explicitly down-level upon join anymore
- Become aware of new pool join rule
- Update Rolling Pool Upgrade

