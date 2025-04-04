---
title: Invocation
description: Invocation of xenguest and the interfaces used for it
weight: 10
mermaid:
  force: true
---
## Interface to xenguest

[xenopsd](../../../) passes this information to [xenguest](index.html)
(for [migration](../../VM.migrate.md), using `emu-manager`):

- The domain type using the command line option `--mode <type>_build`.
- The `domid` of the created empty domain,
- The amount of system memory of the domain,
- A number of other parameters that are domain-specific.

`xenopsd` uses the Xenstore to provide platform data:

- in case the domain has a [VCPUs-mask](../../../../lib/xenctrl/xc_vcpu_setaffinity.md),
  the statically configured vCPU hard-affinity
- the vCPU credit2 weight/cap parameters
- whether the NX bit is exposed
- whether the viridian CPUID leaf is exposed
- whether the system has PAE or not
- whether the system has ACPI or not
- whether the system has nested HVM or not
- whether the system has an HPET or not

When called to build a domain, `xenguest` reads those and builds the VM accordingly.

## Parameters of the VM build modes

{{% include "mode_vm_build.md" %}}
