For development purposes start up several containers preconfigured to run XAPI
in a pool.
This makes developing XAPI and related services that need pool communication
easier, since XAPI can now start without Xen being available (obviously you
won't be able to boot VMs).

'podman' is used since it works rootless and is available across both Debian
and CentOS/Fedora based distributions easily.

Note that this is for development only and the build/runtime environment
doesn't match a XenServer exactly, nor does it intend to.
