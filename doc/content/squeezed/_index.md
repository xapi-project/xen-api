+++
title = "Squeezed"
weight = 50
+++

Squeezed is the XAPI Toolstack's host memory manager (aka balloon driver).
Squeezed uses ballooning to move memory between running VMs, to avoid wasting
host memory.

Principles
----------

1. Avoid wasting host memory: unused memory should be put to use by returning
   it to VMs.
2. Memory should be shared in proportion to the configured policy.
3. Operate entirely at the level of domains (not VMs), and be independent of
   Xen toolstack.
