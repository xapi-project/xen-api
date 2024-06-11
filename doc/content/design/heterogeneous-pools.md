---
title: Heterogeneous pools
layout: default
design_doc: true
revision: 1
status: released (5.6)
---

Notes
=====

-   The `cpuid` instruction is used to obtain a CPU's manufacturer,
    family, model, stepping and features information.
-   The feature bitvector is 128 bits wide: 2 times 32 bits of base
    features plus 2 times 32 bits of extended features, which are
    referred to as `base_ecx`, `base_edx`, `ext_ecx` and `ext_edx`
    (after the registers used by `cpuid` to store the results).
-   The feature bits can be masked by Intel FlexMigration and AMD
    Extended Migration. This means that features can be made to appear
    as absent. Hence, a CPU can appear as a less-capable CPU.
    -   AMD Extended Migration is able to mask both base and extended
        features.
    -   Intel FlexMigration on Core 2 CPUs (Penryn) is able to mask
        **only the base features** (`base_ecx` and `base_edx`). The
        newer Nehalem and Westmere CPUs support extended-feature masking
        as well.
-   A process in dom0 (e.g. xapi) is able to call `cpuid` to obtain the
    (possibly modified) CPU info, or can obtain this information from
    Xen. Masking is done only by Xen at boot time, before any domains
    are loaded.
-   To apply a feature mask, a dom0 process may specify the mask in the
    Xen command line in the file `/boot/extlinux.conf`. After a reboot,
    the mask will be enforced.
-   It is not possible to obtain the original features from a dom0
    process, if the features have been masked. Before applying the first
    mask, the process could remember/store the original feature vector,
    or obtain the information from Xen.
-   All CPU cores on a host can be assumed to be identical. Masking will
    be done simultaneously on all cores in a host.
-   Whether a CPU supports FlexMigration/Extended Migration can (only)
    be derived from the family/model/stepping information.
-   XS5.5 has an exception for the EST feature in base\_ecx. This flag
    is ignored on pool join.

Overview of XenAPI Changes
==========================

Fields
------

Currently, the datamodel has `Host_cpu` objects for each CPU core in a
host. As they are all identical, we are considering keeping just one CPU
record in the `Host` object itself, and deprecating the `Host_cpu`
class. For backwards compatibility, the `Host_cpu` objects will remain
as they are in MNR, but may be removed in subsequent releases.

Hence, there will be a new field called `Host.cpu_info`, a read-only
string-string map, containing the following fixed set of keys:

<table>
<thead>
<tr class="header">
<th align="left">Key name</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>cpu_count</code></td>
<td align="left">The number of CPU cores in the host.</td>
</tr>
<tr class="even">
<td align="left"><code>family</code></td>
<td align="left">The family (number) of the CPU.</td>
</tr>
<tr class="odd">
<td align="left"><code>features</code></td>
<td align="left">The <em>current</em> (possibly masked) feature vector, as given by <code>cpuid</code>. Format: <code>&quot;&lt;base_ecx&gt;-&lt;base_edx&gt;-&lt;ext_ecx&gt;-&lt;ext_edx&gt;&quot;</code>, 4 groups of 8 hexadecimal digits, separated by dashes.</td>
</tr>
<tr class="even">
<td align="left"><code>features_after_reboot</code></td>
<td align="left">The feature vector to be used after rebooting the host. This field can be modified by calling <code>Host.set_cpu_features</code>. Same format as <code>features</code>.</td>
</tr>
<tr class="odd">
<td align="left"><code>flags</code></td>
<td align="left">The flags of the physical CPU (a decoded version of the features field).</td>
</tr>
<tr class="even">
<td align="left"><code>maskable</code></td>
<td align="left">Indicating whether the CPU supports Intel FlexMigration or AMD Extended Migration. There are three possible values: <code>&quot;no&quot;</code> means that masking is not possible, <code>&quot;base&quot;</code> means that only base features can be masked, and <code>&quot;full&quot;</code> means that base as well as extended features can be masked.</td>
</tr>
<tr class="odd">
<td align="left"><code>model</code></td>
<td align="left">The model number of the CPU.</td>
</tr>
<tr class="even">
<td align="left"><code>modelname</code></td>
<td align="left">The model name of the CPU.</td>
</tr>
<tr class="odd">
<td align="left"><code>physical_features</code></td>
<td align="left">The <em>original</em>, unmasked features. Same format as <code>features</code>.</td>
</tr>
<tr class="even">
<td align="left"><code>speed</code></td>
<td align="left">The speed of the CPU.</td>
</tr>
<tr class="odd">
<td align="left"><code>stepping</code></td>
<td align="left">The stepping of the CPU.</td>
</tr>
<tr class="even">
<td align="left"><code>vendor</code></td>
<td align="left">The manufacturer of the CPU.</td>
</tr>
</tbody>
</table>

Indicating whether the CPU supports Intel FlexMigration or AMD Extended
Migration. There are three possible values: `"no"` means that masking is
not possible, `"base"` means that only base features can be masked, and
`"full"` means that base as well as extended features can be masked.

Note: When the `features` and `features_after_reboot` are different,
XenCenter could display a warning saying that a reboot is needed to
enforce the feature masking.

The `Pool.other_config:cpuid_feature_mask` key is recognised. If this
key is present and if it contains a value in the same format as
`Host.cpu_info:features`, the value is used to mask the feature vectors
before comparisons during any pool join in the pool it is defined on.
This can be used to white-list certain feature flags, i.e. to ignore
them when adding a new host to a pool. The default it
`ffffff7f-ffffffff-ffffffff-ffffffff`, which white-lists the EST feature
for compatibility with XS 5.5 and earlier.

Messages
--------

New messages:

-   `Host.set_cpu_features`
    -   Parameters: Host reference `host`, new CPU feature vector
        `features`.
    -   Roles: only Pool Operator and Pool Admin.
    -   Sets the feature vector to be used after a reboot
        (`Host.cpu_info:features_after_reboot`), if `features` is valid.
-   `Host.reset_cpu_features`
    -   Parameter: Host reference `host`.
    -   Roles: only Pool Operator and Pool Admin.
    -   Removes the feature mask, such that after a reboot all features
        of the CPU are enabled.

XAPI
====

Back-end
--------

-   Xen keeps the physical (unmasked) CPU features in memory when
    starts, before applying any masks. Xen exposes the physical
    features, as well as the current (possibly masked) features, to
    dom0/xapi via the function `xc_get_boot_cpufeatures` in libxc.
-   A dom0 script `/etc/xensource/libexec/xen-cmdline`, which provides a
    future-proof way of modifying the Xen command-line key/value pairs.
    This script has the following options, where `mask` is one of
    `cpuid_mask_ecx`, `cpuid_mask_edx`, `cpuid_mask_ext_ecx` or
    `cpuid_mask_ext_edx`, and `value` is `0xhhhhhhhh` (`h` is represents
    a hex digit).:
    -   `--list-cpuid-masks`
    -   `--set-cpuid-masks mask=value mask=value`
    -   `--delete-cpuid-masks mask mask`
-   A `restrict_cpu_masking` key has been added to the host licensing
    restrictions map. This will be `true` when the `Host.edition` is
    `free`, and `false` if it is `enterprise` or `platinum`.

Start-up
--------

The `Host.cpu_info` field is refreshed:

-   The values for the keys `cpu_count`, `vendor`, `speed`, `modelname`,
    `flags`, `stepping`, `model`, and `family` are obtained from
    `/etc/xensource/boot_time_cpus` (and ultimately from
    `/proc/cpuinfo`).
-   The values of the `features` and `physical_features` are obtained
    from Xen and the `features_after_reboot` key is made equal to the
    `features` field.
-   The value of the `maskable` key is determined by the CPU details.
    -   for Intel Core2 (Penryn) CPUs:
        `family = 6 and (model = 1dh or (model = 17h and stepping >= 4))`
        (`maskable = "base"`)
    -   for Intel Nehalem/Westmere CPUs:
        `family = 6 and ((model = 1ah and stepping > 2) or model = 1eh or model = 25h or model = 2ch or model = 2eh or model = 2fh)`
        (`maskable = "full"`)
    -   for AMD CPUs: `family >= 10h` (`maskable = "full"`)

Setting (Masking) and Resetting the CPU Features
------------------------------------------------

-   The `Host.set_cpu_features` call:
    -   checks whether the license of the host is Enterprise or
        Platinum; throws FEATURE\_RESTRICTED if not.
    -   expects a string of 32 hexadecimal digits, optionally containing
        spaces; throws INVALID\_FEATURE\_STRING if malformed.
    -   checks whether the given feature vector can be formed by masking
        the physical feature vector; throws INVALID\_FEATURE\_STRING if
        not. Note that on Intel Core 2 CPUs, it is only possible to the
        mask the base features!
    -   checks whether the CPU supports FlexMigration/Extended
        Migration; throws CPU\_FEATURE\_MASKING\_NOT\_SUPPORTED if not.
    -   sets the value of `features_after_reboot` to the given feature
        vector.
    -   adds the new feature mask to the Xen command-line via the
        `xen-cmdline` script. The mask is represented by one or more of
        the following key/value pairs (where `h` represents a hex
        digit):
        -   `cpuid_mask_ecx=0xhhhhhhhh`
        -   `cpuid_mask_edx=0xhhhhhhhh`
        -   `cpuid_mask_ext_ecx=0xhhhhhhhh`
        -   `cpuid_mask_ext_edx=0xhhhhhhhh`
-   The `Host.reset_cpu_features` call:
    -   copies `physical_features` to `features_after_reboot`.
    -   removes the feature mask from the Xen command-line via the
        `xen-cmdline` script (if any).

Pool Join and Eject
-------------------

-   `Pool.join` fails when the `vendor` and `feature` keys do not match,
    and disregards any other key in `Host.cpu_info`.
    -   However, as XS5.5 disregards the EST flag, there is a new way to
        disregard/ignore feature flags on pool join, by setting a mask
        in `Pool.other_config:cpuid_feature_mask`. The value of this
        field should have the same format as `Host.cpu_info:features`.
        When comparing the CPUID features of the pool and the joining
        host for equality, this mask is applied before the comparison.
        The default is `ffffff7f-ffffffff-ffffffff-ffffffff`, which
        defines the EST feature, bit 7 of the base ecx flags, as "don't
        care".
-   `Pool.eject` clears the database (as usual), and additionally
    removes the feature mask from `/boot/extlinux.conf` (if any).

CLI
===

New commands:

-   `host-cpu-info`
    -   Parameters: `uuid` (optional, uses localhost if absent).
    -   Lists `Host.cpu_info` associated with the host.
-   `host-get-cpu-features`
    -   Parameters: `uuid` (optional, uses localhost if absent).
    -   Returns the value of `Host.cpu_info:features]` associated with
        the host.
-   `host-set-cpu-features`
    -   Parameters: `features` (string of 32 hexadecimal digits,
        optionally containing spaces or dashes), `uuid` (optional, uses
        localhost if absent).
    -   Calls `Host.set_cpu_features`.
-   `host-reset-cpu-features`
    -   Parameters: `uuid` (optional, uses localhost if absent).
    -   Calls `Host.reset_cpu_features`.

The following commands will be deprecated: `host-cpu-list`,
`host-cpu-param-get`, `host-cpu-param-list`.

WARNING:

If the user is able to set any mask they like, they may end up disabling
CPU features that are required by dom0 (and probably other guest OSes),
resulting in a kernel panic when the machine restarts. Hence, using the
set function is potentially dangerous.

It is apparently not easy to find out exactly which flags are safe to
mask and which aren't, so we cannot prevent an API/CLI user from making
mistakes in this way. However, using XenCenter would always be safe, as
XC always copies features masks from real hosts.

If a machine ends up in such a bad state, there is a way to get out of
it. At the boot prompt (before Xen starts), you can type "menu.c32",
select a boot option and alter the Xen command-line to remove the
feature masks, after which the machine will again boot normally (note:
in our set-up, there is first a PXE boot prompt; the second prompt is
the one we mean here).

The API/CLI documentation should stress the potential danger of using
this functionality, and explain how to get out of trouble again.

