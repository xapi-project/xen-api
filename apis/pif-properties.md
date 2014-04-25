GRO and other properties of PIFs
================================

It has been possible to enable and disable GRO and other "ethtool" features on
PIFs for a long time, but there was never an official API for it. Now there is.

Introduction
------------

The former way to enable GRO via the CLI is as follows:

    xe pif-param-set uuid=<pif-uuid> other-config:ethtool-gro=on
    xe pif-plug uuid=<pif-uuid>

The `other-config` field is a grab-bag of options that are not clearly defined.
The options exposed through `other-config` are mostly experimental features, and
the interface is not considered stable. Furthermore, the field is read/write
and does not have any input validation, and cannot not trigger any actions
immediately. The latter is why it is needed to call `pif-plug` after setting
the `ethtool-gro` key, in order to actually make things happen.

New API
-------

New field:
* Field `PIF.properties` of type `(string -> string) map`.
* Physical and bond PIFs have a `gro` key in their `properties`, with possible values `on` and `off`. There are currently no other properties defined.
* VLAN and Tunnel PIFs do not have any properties. They implicitly inherit the properties from the PIF they are based upon (either a physical PIF or a bond).
* For backwards compatibility, if there is a `other-config:ethtool-gro` key present on the PIF, it will be treated as an override of the `gro` key in `PIF.properties`.

New function:
* Message `void PIF.set_property (PIF ref, string, string)`.
 * First argument: the reference of the PIF to act on.
 * Second argument: the key to change in the `properties` field.
 * Third argument: the value to write.
* The function can only be used on physical PIFs that are not bonded, and on bond PIFs. Attempts to call the function on bond slaves, VLAN PIFs, or Tunnel PIFs, fail with `CANNOT_CHANGE_PIF_PROPERTIES`.
* Calls with invalid keys or values fail with `INVALID_VALUE`.
* When called on a bond PIF, the key in the `properties` of the associated bond slaves will also be set to same value.
* The function automatically causes the settings to be applied to the network devices (no additional `plug` is needed). This includes any VLANs that are on top of the PIF to-be-changed, as well as any bond slaves.

Defaults, Installation and Upgrade
------------------------

* Any newly introduced PIF will have its `properties` field set to `"gro" -> "on"`. This includes PIFs obtained after a fresh installation of XenServer, as well as PIFs created using `PIF.introduce` or `PIF.scan`. In other words, GRO will be "on" by default.
* An upgrade from a version of XenServer that does not have the `PIF.properties` field, will give every physical and bond PIF a `properties` field set to `"gro" -> "on"`. In other words, GRO will be "on" by default after an upgrade.

Bonding
-------

* When creating a bond, the bond-slaves-to-be must all have equal `PIF.properties`. If not, the `bond.create` call will fail with `INCOMPATIBLE_BOND_PROPERTIES`.
* When a bond is created successfully, the `properties` of the bond PIF will be equal to the properties of the bond slaves.

Command Line Interface
----------------------

* The `PIF.properties` field is exposed through `xe pif-list` and `xe pif-param-list` as usual.
* The `PIF.set_property` call is exposed through `xe pif-param-set`. For example: `xe pif-param-set uuid=<pif-uuid> properties:gro=off`.
