---
title: VM_build micro-op
linkTitle: VM_build μ-op
description: Overview of the VM_build μ-op (runs after the VM_create μ-op created the domain).
weight: 10
mermaid:
  force: true
---

## Overview

On Xen, `Xenctrl.domain_create` creates an empty domain and
returns the domain ID (`domid`) of the new domain to `xenopsd`.

In the `build` phase, the `xenguest` program is called to create
the system memory layout of the domain, set vCPU affinity and a
lot more.

The [VM_build](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/lib/xenops_server.ml#L2255-L2271)
micro-op collects the VM build parameters and calls
[VM.build](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/xc/xenops_server_xen.ml#L2290-L2291),
which calls
[VM.build_domain](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/xc/xenops_server_xen.ml#L2250-L2288),
which calls
[VM.build_domain_exn](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/xc/xenops_server_xen.ml#L2024-L2248)
which calls [Domain.build](Domain.build):

{{% include "VM_build-chart.md" %}}

The function
[VM.build_domain_exn](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/xc/xenops_server_xen.ml#L2024)
must:

1. Run pygrub (or eliloader) to extract the kernel and initrd, if necessary
2. [Call](https://github.com/xapi-project/xen-api/blob/master/ocaml/xenopsd/xc/xenops_server_xen.ml#L2222-L2225)
   [Domain.build](Domain.build)
   to:
   - optionally run NUMA placement and
   - invoke [xenguest](VM.build/xenguest) to set up the domain memory.

   See the walk-though on [VM.build](VM.build) for more details on this phase.
3. Apply the `cpuid` configuration
4. Store the current domain configuration on disk -- it's important to know
   the difference between the configuration you started with and the configuration
   you would use after a reboot because some properties (such as maximum memory
   and vCPUs) as fixed on create.
