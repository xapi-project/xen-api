+++
title = "Xapi-guard"
weight = 50
+++

The `xapi-guard` daemon is the component in the xapi toolstack that is responsible for handling persistence requests from VMs (domains).
Currently these are UEFI vars and vTPM updates.

The code is in `ocaml/xapi-guard`.
When the daemon managed only with UEFI updates it was called `varstored-guard`.
Some files and package names still use the previous name.

Principles
----------
1. Calls from domains must be limited in privilege to do certain API calls, and
   to read and write from their corresponding VM in xapi's database only.
2. Xenopsd is able to control xapi-guard through message switch, this access is
   not limited.
3. Listening to domain socket is restored whenever the daemon restarts to minimize disruption of running domains.


Overview
--------

Xapi-guard forwards calls from domains to xapi to persist UEFI variables, and update vTPMs.
To do this, it listens to 1 socket per service (varstored, or swtpm) per domain.
To create these sockets before the domains are running, it listens to a message-switch socket.
This socket listens to calls from xenopsd, which orchestrates the domain creation.
