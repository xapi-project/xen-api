+++
title = "Live Migration Sequence Diagram"
linkTitle = "Live Migration"
+++

{{<mermaid align="left">}}
sequenceDiagram
autonumber
participant tx as sender
participant rx0 as receiver thread 0
participant rx1 as receiver thread 1
participant rx2 as receiver thread 2

activate tx
tx->>rx0: VM.import_metadata
tx->>tx: Squash memory to dynamic-min

tx->>rx1: HTTP /migrate/vm
activate rx1
rx1->>rx1: VM_receive_memory<br/>VM_create (00000001)<br/>VM_restore_vifs
rx1->>tx: handshake (control channel)<br/>Synchronisation point 1

tx->>rx2: HTTP /migrate/mem
activate rx2
rx2->>tx: handshake (memory channel)<br/>Synchronisation point 1-mem

tx->>rx1: handshake (control channel)<br/>Synchronisation point 1-mem ACK

rx2->>rx1: memory fd

tx->>rx1: VM_save/VM_restore<br/>Synchronisation point 2
tx->>tx: VM_rename
rx1->>rx2: exit
deactivate rx2

tx->>rx1: handshake (control channel)<br/>Synchronisation point 3

rx1->>rx1: VM_rename<br/>VM_restore_devices<br/>VM_unpause<br/>VM_set_domain_action_request

rx1->>tx: handshake (control channel)<br/>Synchronisation point 4

deactivate rx1

tx->>tx: VM_shutdown<br/>VM_remove
deactivate tx

{{< /mermaid >}}
