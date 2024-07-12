---
title: FCoE capable NICs
layout: default
design_doc: true
revision: 3
status: proposed
design_review: 120
---

It has been possible to identify the NICs of a Host which can support FCoE.
This property can be listed in PIF object under capabilities field.

Introduction
------------

* FCoE supported on a NIC is a hardware property. With the help of dcbtool, we can identify which NIC support FCoE.
* The new field capabilities will be `Set(String)` in PIF object. For FCoE capable NIC will have string "fcoe" in PIF capabilities field.
* `capabilities` field will be ReadOnly, This field cannot be modified by user.

PIF Object
-------

New field:

* Field `PIF.capabilities` will be type `Set(string)`.
* Default value in PIF capabilities will have an empty set.

Xapi Changes
-------

* Set the field capabilities "fcoe" depending on output of xcp-networkd call `get_capabilities`.
* Field capabilities "fcoe" can be set during `introduce_internal` on when creating a PIF.
* Field capabilities "fcoe" can be updated during `refresh_all` on xapi startup.
* The above field will be set everytime when xapi-restart.

XCP-Networkd Changes
-------

New function:

* String list `string list get_capabilties (string)`
* Argument: device_name for the PIF.
* This function calls method `capable` exposed by `fcoe_driver.py` as part of dom0.
* It returns string list ["fcoe"] or [] depending on `capable` method output.

Defaults, Installation and Upgrade
------------------------

* Any newly introduced PIF will have its capabilities field as empty set until `fcoe_driver` method `capable` states FCoE is supported on the NIC.
* It includes PIFs obtained after a fresh install of Xenserver, as well as PIFs created using `PIF.introduce` then `PIF.scan`.
* During an upgrade Xapi Restart will call `refresh_all` which then populate the capabilities field as empty set.

Command Line Interface
----------------------

* The `PIF.capabilities` field is exposed through `xe pif-list` and `xe pif-param-list` as usual.
