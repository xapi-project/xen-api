XE CLI + bash examples

 [install-debian][1]

   A very simple example which installs Debian Etch 4.0 from the pre-existing
   VM template.

[suspend-resume][2]

   A very simple example which demonstrates how to suspend and resume a VM.

[clone-vms][3]

   An example which takes the uuid of a VM which is either running or halted.
   If it is running it is shutdown. The VM is cloned and if the original VM was
   running then both it and the clone are started.

[move-management-to-bond][4]

   An example which detects when some hosts management interfaces are still
   using a regular interface when a suitable bond configuration exists, and
   moves the management interfaces to the bond.


[1]: install-debian
[2]: suspend-resume
[3]: clone-vms
[4]: move-management-to-bond
