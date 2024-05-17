---
title: RDP control
layout: default
design_doc: true
revision: 2
status: released (XenServer 6.5 SP1)
design_review: 12
---
### Purpose

To administer guest VMs it can be useful to connect to them over Remote Desktop Protocol (RDP). XenCenter supports this; it has an integrated RDP client.

First it is necessary to turn on the RDP service in the guest.

This can be controlled from XenCenter. Several layers are involved. This description starts in the guest and works up the stack to XenCenter.

This feature was completed in the first quarter of 2015, and released in Service Pack 1 for XenServer 6.5.

### The guest agent

The XenServer guest agent installed in Windows VMs can turn the RDP service on and off, and can report whether it is running.

The guest agent is at https://github.com/xenserver/win-xenguestagent

Interaction with the agent is done through some Xenstore keys:

The guest agent running in domain N writes two xenstore nodes when it starts up:
* `/local/domain/N/control/feature-ts = 1`
* `/local/domain/N/control/feature-ts2 = 1`

This indicates support for the rest of the functionality described below.

(The "...ts2" flag is new for this feature; older versions of the guest agent wrote the "...ts" flag and had support for only a subset of the functionality (no firewall modification), and had a bug in updating `.../data/ts`.)

To indicate whether RDP is running, the guest agent writes the string "1" (running) or "0" (disabled) to xenstore node

`/local/domain/N/data/ts`.

It does this on start-up, and also in response to the deletion of that node.

The guest agent also watches xenstore node `/local/domain/N/control/ts` and it turns RDP on and off in response to "1" or "0" (respectively) being written to that node. The agent acknowledges the request by deleting the node, and afterwards it deletes `local/domain/N/data/ts`, thus triggering itself to update that node as described above.

When the guest agent turns the RDP service on/off, it also modifies the standard Windows firewall to allow/forbid incoming connections to the RDP port. This is the same as the firewall change that happens automatically when the RDP service is turned on/off through the standard Windows GUI.

### XAPI etc.

xenopsd sets up watches on xenstore nodes including the `control` tree and `data/ts`, and prompts xapi to react by updating the relevant VM guest metrics record, which is available through a XenAPI call.

XenAPI includes a new message (function call) which can be used to ask the guest agent to turn RDP on and off.

This is `VM.call_plugin` (analogous to `Host.call_plugin`) in the hope that it can be used for other purposes in the future, even though for now it does not really call a plugin.

To use it, supply `plugin="guest-agent-operation"` and either `fn="request_rdp_on"` or `fn="request_rdp_off"`.

See http://xapi-project.github.io/xen-api/classes/vm.html

The function strings are named with "request" (rather than, say, "enable_rdp" or "turn_rdp_on") to make it clear that xapi only makes a request of the guest: when one of these calls returns successfully this means only that the appropriate string (1 or 0) was written to the `control/ts` node and it is up to the guest whether it responds.

### XenCenter

#### Behaviour on older XenServer versions that do not support RDP control

Note that the current behaviour depends on some global options: "Enable Remote Desktop console scanning" and "Automatically switch to the Remote Desktop console when it becomes available".

1.    When tools are not installed:
  *       As of XenCenter 6.5, the RDP button is absent.
2.    When tools are installed but RDP is not switched on in the guest:
  1.      If "Enable Remote Desktop console scanning" is on:
    *         The RDP button is present but greyed out. (It seems to sometimes read "Switch to Remote Desktop" and sometimes read "Looking for guest console...": I haven't yet worked out the difference).
    *         We scan the RDP port to detect when RDP is turned on
  2.      If "Enable Remote Desktop console scanning" is off:
    *         The RDP button is enabled and reads "Switch to Remote Desktop"
3.    When tools are installed and RDP is switched on in the guest:
  1.      If  "Enable Remote Desktop console scanning" is on:
    *         The RDP button is enabled and reads "Switch to Remote Desktop"
    *         If "Automatically switch" is on, we switch to RDP immediately we detect it
  2.      If "Enable Remote Desktop console scanning" is off:
    *         As above, the RDP button is enabled and reads "Switch to Remote Desktop"

#### New behaviour on XenServer versions that support RDP control

1. This new XenCenter behaviour is only for XenServer versions that support RDP control, with guests with the new guest agent: behaviour must be unchanged if the server or guest-agent is older.
2. There should be no change in the behaviour for Linux guests, either PV or HVM varieties: this must be tested.
3. We should never scan the RDP port; instead we should watch for a change in the relevant variable in guest_metrics.
4. The XenCenter option "Enable Remote Desktop console scanning" should change to read "Enable Remote Desktop console scanning (XenServer 6.5 and earlier)"
5. The XenCenter option "Automatically switch to the Remote Desktop console when it becomes available" should be enabled even when "Enable Remote Desktop console scanning" is off.
6. When tools are not installed:
  * As above, the RDP button should be absent.
7. When tools are installed but RDP is not switched on in the guest:
  * The RDP button should be enabled and read "Turn on Remote Desktop"
  * If pressed, it should launch a dialog with the following wording: "Would you like to turn on Remote Desktop in this VM, and then connect to it over Remote Desktop?   [Yes] [No]"
  * That button should turn on RDP, wait for RDP to become enabled, and switch to an RDP connection. It should do this even if "Automatically switch" is off.
8. When tools are installed and RDP is switched on in the guest:
  * The RDP button should be enabled and read "Switch to Remote Desktop"
  * If "Automatically switch" is on, we should switch to RDP immediately
  * There is no need for us to provide UI to switch RDP off again
9. We should also test the case where RDP has been switched on in the guest before the tools are installed.

