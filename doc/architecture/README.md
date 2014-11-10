Xapi architecture
=================

Xapi is responsible for
- providing a stable interface (the XenAPI)
- hosting the "xe" CLI
- allowing one client to manage multiple hosts
- authenticating users and applying role-based access control
- locking resources (in particular disks)
- allowing storage to be managed through plugins
- planning and coping with host failures ("High Availability")
- storing VM and host configuration
- generating alerts
- managing software patching

Note that Xapi delegates core functions such as
- starting VMs to [Xenopsd](https://github.com/xapi-project/xenopsd)
- monitoring performance counters to [Xcp-rrdd](https://github.com/xapi-project/xcp-rrdd)
- setting up host networking to [Xcp-networkd](https://github.com/xapi-project/xcp-networkd)

The following diagram shows the internals of Xapi:

![Internals of xapi](http://xapi-project.github.io/xen-api/doc/architecture/xapi.png)

The top of the diagram shows the XenAPI clients: XenCenter, XenOrchestra, OpenStack and
CloudStack using XenAPI and HTTP GET/PUT over ports 80 and 443 to talk to xapi.
These XenAPI (XMLRPC over HTTP POST) and HTTP GET/PUT are always authenticated
using either PAM (by default using the local passwd and group files) or through
Active Directory.

The APIs are classified into categories:
* master-only: these are the majority of current APIs. The master should be called and
  relied upon to forward the call to the right place with the right locks held.
* normally-local: these are performance special cases such as disk import/export
  and console connection which are sent directly to hosts which have the most
  efficient access to the data.
* emergency: these deal with scenarios where the master is offline

If the incoming API call should be resent to the master than a XenAPI ```HOST_IS_SLAVE```
error message containing the master's IP is sent to the client.

Once past the initial checks, API calls enter the "message forwarding" layer which
- locks resources (via the ```current_operations``` mechanism)
- decides which host should execute the request.

If the request should run locally then a direct function call is used; otherwise
the message forwarding code makes a synchronous API call to a specific other host.
Note: Xapi currently employs a "thread per request" model which causes one full POSIX thread
to be created for every request. Even when a request is forwarded the full thread
persists, blocking for the result to become available.

If the XenAPI call is a VM lifecycle operation then it is converted into a Xenopsd
API call and forwarded over a Unix domain socket. Xapi and Xenopsd have similar
notions of cancellable asynchronous "tasks", so the current Xapi task (all operations
run in the context of a task) is bound to the Xenopsd task, so cancellation is
passed through and progress updates are received.

If the XenAPI call is a storage operation then the "storage access" layer
- verifies that the storage objects are in the correct state (SR attached/detached;
  VDI attached/activated read-only/read-write)
- invokes the relevant operation in the Storage Manager API (SMAPI) v2 interface;
- uses the SMAPIv2 to SMAPIv1 converter to generate the necessary command-line to talk to
  the SMAPIv1 plugin (EXT, NFS, LVM etc) and to execute it
- persists the state of the storage objects (including the result of a ```VDI.attach```
  call) to persistent storage

Internally the SMAPIv1 plugins use privileged access to the Xapi database to directly
set fields (e.g. VDI.virtual_size) that would be considered read/only to other clients.
The SMAPIv1 plugins also rely on Xapi for
- knowledge of all hosts which may access the storage
- locking of disks within the resource pool
- safely executing code on other hosts via the "Xapi plugin" mechanism

The Xapi database contains Host and VM metadata and is shared pool-wide. The master
keeps a copy in memory, and all other nodes remote queries to the master. The database
associates each object with a generation count which is used to implement the XenAPI
```event.next``` and ```event.from``` APIs. The database is routinely asynchronously flushed to disk
in XML format. If the "redo-log" is enabled then all database writes are made synchronously
as deltas to a shared block device. Without the redo-log, recent updates may be lost
if Xapi is killed before a flush.

High-Availability refers to planning for host failure, monitoring host liveness and then
following-through on the plans. Xapi defers to an external host liveness monitor
called ```xhad```. When ```xhad``` confirms that a host has failed -- and has been
isolated from the storage -- then Xapi will restart any VMs which have failed and which
have been marked as "protected" by HA. Xapi can also impose admission control to prevent
the pool becoming too overloaded to cope with ```n``` arbitrary host failures.

The ```xe``` CLI is implemented in terms of the XenAPI, but for efficiency the implementation
is linked directly into Xapi. The ```xe``` program remotes its command-line to Xapi,
and Xapi sends back a series of simple commands (prompt for input; print line; fetch file;
exit etc).
