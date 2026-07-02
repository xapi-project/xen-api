---
title: VLAN filtering support
layout: default
design_doc: true
revision: 1
status: proposed
---

<!--toc:start-->
- [Overview](#overview)
- [Use Cases](#use-cases)
- [Changes](#changes)
  - [Database schema](#database-schema)
  - [API](#api)
  - [Behavior change](#behavior-change)
- [Possible designs](#possible-designs)
  - [Using VIF](#using-vif)
  - [Using Network](#using-network)
  - [Using PIF](#using-pif)
  - [Using new object](#using-new-object)
  - [Conclusion](#conclusion)
- [Glossary](#glossary)
<!--toc:end-->

## Overview

VLAN filtering is a Layer 2 network segmentation mechanism that controls how Ethernet frames are forwarded across switch ports based on VLAN membership.
It enables a single physical switching infrastructure to support multiple isolated broadcast domains while maintaining traffic separation and policy enforcement.

When VLAN filtering is enabled, the switch evaluates incoming frames against configured VLAN rules before allowing traffic to traverse the network.
Frames are sent either tagged (IEEE 802.1Q) or untagged depending on port configuration requirements.

VLAN filtering is commonly used to isolate network segments, and provides only partial view of a trunk link to VM.

## Use Cases

The general use case is providing trunk to some VM in multi-tenants configuration.
For example, having a VM firewall with VLAN trunking but seeing only a subset of the VLANs present on the trunk.

As examples, the following configurations could be considered,
with one host having a PIF interface with the following VLANs \[10,11,12,13\] on it :

* VM-A with VIF as trunk port in xenbr0 bridge, `trunks=[]` : VM-A sees all tagged packets so from VLANs 10 to 13 (what we are able to do currently)
* VM-B with VIF as access port in xenbr0 bridge, `tag=10` : VM-B sees untagged packets from VLAN 10 (what we are able to do currently)
* VM-C with VIF as trunk port in xenbr0 bridge, `trunks=[10,11]` : VM-C sees tagged packets from VLANs 10 and 11
* VM-D with VIF as trunk port in xenbr0 bridge, `trunks=[11,12]` : VM-D sees tagged packets from VLANs 11 and 12
* VM-E with VIF as trunk port in xenbr0 bridge, `trunks=[10]` : VM-E sees tagged packets from VLAN 10

## Changes

### Database schema

The *VIF* class would be extended with a new attribute:

* "trunks" (set int, default to empty): the 802.1Q VLANs that this port trunks (if available) ; if it is empty, then the port trunks all VLANs.

### API

This is a new API introduced to manage `trunks` attribute.

* VIF.add_trunks
  * self (ref VIF): reference to a valid VIF;
  * value (int): The 802.1Q VLAN which will be associated with the VIF.

* VIF.remove_trunks
  * self (ref VIF): reference to a valid VIF;
  * value (int): the 802.1Q VLAN which will be removed from the VIF.

* VIF.set_trunks
  * self (ref VIF): reference to a valid VIF;
  * value (set int): The 802.1Q VLANs which will be associated with the VIF.

### Behavior change

When a VIF is created, *trunks* attribute on VIF is synchronized to `trunks` attribute on `Port` table in OpenvSwitch.
As the empty list is the current default in OpenvSwitch,
it doesn't introduce changes from current behaviour when default value is used.

The `trunks` attribute on `Port` table is kept synchronized with *trunks* attribute on VIF during all the lifecycle of the port.

From <https://www.openvswitch.org/support/dist-docs/ovs-vswitchd.conf.db.5.html>:

> **trunks**: set of up to 4,096 integers, in range 0 to 4,095
> For a trunk, native-tagged, or native-untagged port, the 802.1Q VLAN or VLANs that this port trunks;
> if it is empty, then the port trunks all VLANs.
> Must be empty if this is an access port.

The type of the port is defined by `vlan_mode` column on the `Port` table.

As in XAPI we don't set it, we are using the default mode defined as following:

> * If tag contains a value, the port is an access port. The trunks column should be empty.
> * Otherwise, the port is a trunk port. The trunks column value is honored if it is present.

The `tag` in OpenvSwitch is derived from the **PIF's VLAN tag** on the VIF's Network.
For consistency with OpenvSwitch, the `trunks` attribute is so expected to be empty if tag is also set.
In XAPI term it means that a VIF with not empty *trunks* attribute could only be associated to not VLAN Network (Network with PIF with VLAN = -1).

This introduces a validation constraint preventing incompatible configurations:

* A VIF with non-empty `trunks` cannot be associated with a Network backed by a VLAN-tagged PIF (`PIF.vLAN` ≠ -1).
* If a VIF is already associated with a Network backed by a VLAN-tagged PIF, its `trunks` attribute must remain empty.
* A VLAN-tagged PIF (`PIF.vLAN` ≠ -1) cannot be associated with a Network that contains a VIF with non-empty `trunks`.

### Impacts

#### Update from older version

All VIFs will get a new *trunks* attribute which will be the default value (empty set).
It doesn't introduce any behavior changes.

#### VM start

For each created VIF, the value of *trunks* attribute will be set to `trunks` attribute on OpenvSwitch `Port` table.

#### VM migration

If the VM is migrated, the `trunks` attribute on OpenvSwitch `Port` table on the new host is keep in synchronization with the value of *trunks* attribute.

#### VIF attribute changes

On *trunks* attribute change, the value will be set to `trunks` attribute on OpenvSwitch `Port` table.
The change is effective immediately and transparently (without replugging the VIF).


## Possible designs

The proposed design was chosen after considering the following elements.
They are taken up here to present the possible options and reasons for choosing to use VIF.

### Using VIF

* **pros**
  * simplest implementation path : only a new attribute on VIF to synchronize with OpenvSwitch `Port` configuration
  * per-VIF granularity is a feature here : each VM gets its own filtered view, matching the multi-tenant use case
  * no need to change existing XAPI invariants (one PIF, one Network, see below)
  * the attribute lives on the object that actually needs the filter — no scaffolding objects required
* **cons**
  * doesn't reuse XAPI's existing VLAN model (VLAN -> tagged PIF -> Network chain), so the concept is somewhat duplicated
  * configuration is scoped to a single VIF : sharing the same trunk policy across multiple VIFs would require manual duplication
  * visibility is more limited than a dedicated Network : admins looking at network-level configuration won't immediately see which VLANs are being filtered

### Using Network

* **pros**
  * natural scoping : the trunk policy would be easily discoverable
  * consistent with how other per-network settings (MTU, locking mode, etc.) are already attached
* **cons**
  * a single PIF would need to be a member of several Networks simultaneously, which is conceptually problematic (a PIF normally belongs to exactly one Network)
  * would require rethinking or relaxing the "one PIF, one Network" invariant in XAPI

### Using PIF

* **pros**
  * PIF already has a VLAN-aware model via `vlan-slave-of` (the set of VLANs attached to that PIF)
  * could express the "filtered view of available VLANs" at the physical interface level
* **cons**
  * we would need several PIF backends on the same physical device (one PIF per trunk view), which contradicts the one-backing-device model
  * does not fit well with VLAN tagging or bonding on top of the PIF ; would need additional restrictions (e.g. blocking VLAN or Bond creation on a PIF that already has trunked views)

### Using new object

* **pros**
  * clean semantic : a dedicated object expresses "a PIF + a subset of VLANs" without overloading existing entities
  * could naturally group multiple VIFs under the same trunk policy (like a Network does)
* **cons**
  * seems overkill for the use case : adds a new object type, new API surface, and new lifecycle concerns
  * would force creating a VLAN PIF / Network for each trunk subset, adding scaffolding noise for what is essentially a per-VIF filter

### Conclusion

It was chosen to put `trunks` attribute on VIF as:

* it is simple design solution
* an installation usage trunks will not have many VIFs sharing the same configuration

The well suited alternative would be to create a new object (like VLAN or Bond) for holding the trunk information, but it seems overkill for the purpose.

Other alternatives would need more changes in current XAPI invariants.

## Glossary

* **VLAN (Virtual LAN)** : A logical subdivision of a physical network that isolates traffic at Layer 2. VLANs are identified by a 12-bit ID (range 0–4095).
* **VLAN tag** : A 4-byte field inserted into an Ethernet frame by the switch, containing a 12-bit VLAN ID (0–4095).
* **802.1Q** : IEEE standard defining a system of VLAN tagging for Ethernet frames — adds a VLAN identifier to Ethernet frames to segregate traffic at Layer 2.
* **Access port** : An OVS port type that carries traffic for a single VLAN. The `trunks` column must be empty; the `tag` column specifies the VLAN. Packets are untagged.
* **Trunk port** : An OVS port type that carries traffic for multiple VLANs. The `trunks` column specifies which VLANs are allowed; if empty, all VLANs are allowed. Packets are tagged.
* **Trunk link** : A physical or virtual link that carries traffic for multiple VLANs simultaneously. Packets are tagged.
