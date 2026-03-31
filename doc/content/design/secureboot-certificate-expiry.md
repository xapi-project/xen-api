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

### 2.3 RBAC

The new update API follows VM-admin-level access, aligned with existing NVRAM-related VM operations.

## 3. Design for Components

### 3.1 VM Certificate State Model

`VM.secureboot_certificates_state` applies to VM-class objects, including:

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

### 3.4 NVRAM and State Consistency

The certificate state must stay consistent with actual NVRAM content.

Key interface change:

- Extend `VM.set_NVRAM_EFI_variables` with optional parameter `update`

Rules:

- `update=yes` -> set state `update_available`
- `update=no` -> do not update state
- omitted -> xapi runs certificate check helper and derives state

This ensures compatibility when old varstored instances are still running during rolling update windows.

### 3.5 Boot-time Automatic Update Path

When varstored initializes a VM and sees `secureboot_certificates_state=update_on_boot`:

- Perform certificate update flow during boot-time initialization
- Write updated NVRAM and synchronize state via `VM.set_NVRAM_EFI_variables`

### 3.6 End-to-end Workflow

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
