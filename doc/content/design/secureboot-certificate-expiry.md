---
title: Handling Microsoft Secure Boot Certificate Expiry
layout: default
design_doc: true
revision: 1
status: draft
---

## 1. Background

Microsoft Secure Boot certificates from 2011 are reaching end-of-life, and legacy VMs may still contain only the old certificate set. XenServer needs an out-of-band mechanism to update per-VM UEFI Secure Boot variables safely and at scale.

Scope of this design:

- Update certificate state tracking and update flow for VMs, snapshots, and templates
- Provide API support for scheduling certificate updates on VM boot
- Integrate xapi and varstored behavior for consistent state handling

## 2. System Overview

### 2.1 Out-of-band Update Mechanism

Certificate update is implemented as a dedicated API-driven workflow (not a plugin), so that:

- The interface is documented and SDK-generated
- RBAC can be assigned precisely
- xapi can route requests and coordinate host-side behavior consistently

### 2.2 Certificate State Tracking

A new VM field is introduced:

- `VM.secureboot_certificates_state` (enum, readonly)

States:

- `ok`: No update required (including non-applicable VM types)
- `update_available`: Update required
- `update_on_boot`: Update scheduled for next boot

~~~mermaid

stateDiagram
update_available --> update_on_boot : Admin marks VM for update
update_on_boot --> ok : VM boots, update succeeds
update_on_boot --> update_on_boot : VM boots, update fails(retain state)
ok --> update_available : recompute state(e.g. legacy VM import)

~~~

### 2.3 RBAC

The new update API follows VM-admin-level access, aligned with existing NVRAM-related VM operations.

## 3. Design for Components

### 3.1 VM Certificate State Model

`VM.secureboot_certificates_state` applies to these VM-class objects,

- VMs
- Snapshots
- Templates

Transition intent:

- Admin marks a VM for update: `update_available -> update_on_boot`
- VM boots and update succeeds: `update_on_boot -> ok`
- VM boots and update fails: remains `update_on_boot` or is reset to `update_available` based on update result handling

### 3.2 API: Mark/Unmark Update-on-Boot

New API:

- `VM.update_secureboot_certificates_on_boot(session, vm, mark)`

Behavior:

- `mark=true`: require current state `update_available`, then set `update_on_boot`
- `mark=false`: require current state `update_on_boot`, then set `update_available`

Validation:

- Reject invalid transitions with `OPERATION_NOT_ALLOWED`

### 3.3 DB Upgrade and Import Handling

On toolstack restart after upgrade:

- Initialize `secureboot_certificates_state` for all VM records to `ok`
- Re-evaluate NVRAM and set `update_available` where needed

Applied to:

- VMs
- Snapshots
- Non-default templates

Default templates remain `ok`.

For VM import and cross-pool migration:

- If imported metadata lacks `secureboot_certificates_state`, determine state from NVRAM and set it during import
- If imported metadata contains `secureboot_certificates_state`, reserve the state during import

### 3.4 NVRAM and State Consistency

The certificate state must stay consistent with actual NVRAM content.

Key interface change:

- Extend `VM.set_NVRAM_EFI_variables` with optional parameter `update`, we call it `VM.set_NVRAM_EFI_variables_V2`

Rules:

- `update=yes` -> set state `ok`
- `update=no` -> do not update state
- omitted -> xapi runs certificate check helper and derives state

This ensures compatibility when old varstored instances are still running during rolling update windows.

### 3.5 Certificate Check Helper

A standalone program  will be introduced, which xapi calls to determine the SecureBoot cert state

Inputs:

- `temp file path` which contains NVRAM EFI-variables data

Behavior:

- This program comes to use some common functions shared with varstored.
- This program is launched by xapi, it is executed in a sandboxed and reduced privileges environment.
- Xapi retrieves VM's NVRAM content from database and passes it to this program via command-line arguments.
- If this program outputs `update_required`, xapi sets `VM.secureboot_certificates_state` to be `update_available`.
- If this program outputs `update_ok`, xapi sets `VM.secureboot_certificates_state` to be `ok`.
- On toolstack restart, during DB upgrade, this program is invoked to compute `VM.secureboot_certificates_state`. Since xapi process has not completed initialization at that point, this program cannot call any services of xapi.

### 3.6 Boot-time Automatic Update Path

When varstored initializes a VM and sees `secureboot_certificates_state=update_on_boot`, varstored does,

- Perform certificate update flow during boot-time initialization
- Write updated NVRAM and synchronize state via `VM.set_NVRAM_EFI_variables_V2`

The `VM.set_NVRAM_EFI_variables_V2` interface performs same as `VM.set_NVRAM_EFI_variables`, uses the existing varstored-guard process to make calls to xapi.

If `VM.set_NVRAM_EFI_variables_V2` runs into error (e.g. there is something wrong with the communication with xapi),

- xapi does not update VM NVRAM and `VM.secureboot_certificates_state`
- VM boot gets stuck at the firmware initialization stage, if the issue is not fixed, rebooting the VM will still encounter the same problem
- Once the issue is fixed, admin can continue the secureboot certificate upgrade by VM reboot

### 3.7 End-to-end Workflow

1. Upgrade packages (`xapi-core`, `varstored`, related components)
2. Restart toolstack
3. xapi DB upgrade initializes and recalculates `secureboot_certificates_state`
4. Admin marks selected VMs via `VM.update_secureboot_certificates_on_boot`
5. VM reboot triggers varstored certificate update
6. xapi updates state to reflect post-update NVRAM content

## 4. Out of Scope

- User-notification mechanism for certificate expiry
- Custom certificate workflow
- Template/snapshot feature expansion beyond state tracking and conversion behavior
- OS-specific test-process guidance
- VM with Secure Boot PCR7 binding (e.g. Windows bitlocker), provide customer documentation to guide how to resolve such issues
