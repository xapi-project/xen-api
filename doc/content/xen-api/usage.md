+++
title = "Using the API"
weight = 50
+++

This chapter describes how to use the XenServer Management API from real programs to manage XenServer Hosts and VMs. The chapter begins with a walk-through of a typical client application and demonstrates how the API can be used to perform common tasks. Example code fragments are given in python syntax but equivalent code in the other programming languages would look very similar. The language bindings themselves are discussed afterwards and the chapter finishes with walk-throughs of two complete examples.

Anatomy of a typical application
--------------------------------

This section describes the structure of a typical application using the XenServer Management API. Most client applications begin by connecting to a XenServer Host and authenticating (e.g. with a username and password). Assuming the authentication succeeds, the server will create a "session" object and return a reference to the client. This reference will be passed as an argument to all future API calls. Once authenticated, the client may search for references to other useful objects (e.g. XenServer Hosts, VMs, etc.) and invoke operations on them. Operations may be invoked either synchronously or asynchronously; special task objects represent the state and progress of asynchronous operations. These application elements are all described in detail in the following sections.

### Choosing a low-level transport

API calls can be issued over two transports:

-   SSL-encrypted TCP on port 443 (https) over an IP network

-   plaintext over a local Unix domain socket: `/var/xapi/xapi`

The SSL-encrypted TCP transport is used for all off-host traffic while the Unix domain socket can be used from services running directly on the XenServer Host itself. In the SSL-encrypted TCP transport, all API calls should be directed at the Resource Pool master; failure to do so will result in the error `HOST_IS_SLAVE`, which includes the IP address of the master as an error parameter.

Because the master host of a pool can change, especially if HA is enabled on a pool, clients must implement the following steps to detect a master host change and connect to the new master as required:

Subscribe to updates in the list of hosts servers, and maintain a current list of hosts in the pool

If the connection to the pool master fails to respond, attempt to connect to all hosts in the list until one responds

The first host to respond will return the `HOST_IS_SLAVE` error message, which contains the identity of the new pool master (unless of course the host is the new master)

Connect to the new master

> **Note**
>
> As a special-case, all messages sent through the Unix domain socket are transparently forwarded to the correct node.

### Authentication and session handling

The vast majority of API calls take a session reference as their first parameter; failure to supply a valid reference will result in a `SESSION_INVALID` error being returned. Acquire a session reference by supplying a username and password to the `login_with_password` function.

> **Note**
>
> As a special-case, if this call is executed over the local Unix domain socket then the username and password are ignored and the call always succeeds.

Every session has an associated "last active" timestamp which is updated on every API call. The server software currently has a built-in limit of 500 active sessions and will remove those with the oldest "last active" field if this limit is exceeded for a given `username` or `originator`. In addition all sessions whose "last active" field is older than 24 hours are also removed. Therefore it is important to:

-   Specify an appropriate `originator` when logging in; and

-   Remember to log out of active sessions to avoid leaking them; and

-   Be prepared to log in again to the server if a `SESSION_INVALID` error is caught.

In the following Python fragment a connection is established over the Unix domain socket and a session is created:

    import XenAPI

        session = XenAPI.xapi_local()
        try:
            session.xenapi.login_with_password("root", "", "2.3", "My Widget v0.1")
            ...
        finally:
            session.xenapi.session.logout()

### Finding references to useful objects

Once an application has authenticated the next step is to acquire references to objects in order to query their state or invoke operations on them. All objects have a set of "implicit" messages which include the following:

-   `get_by_name_label` : return a list of all objects of a particular class with a particular label;

-   `get_by_uuid` : return a single object named by its UUID;

-   `get_all` : return a set of references to all objects of a particular class; and

-   `get_all_records` : return a map of reference to records for each object of a particular class.

For example, to list all hosts:

    hosts = session.xenapi.host.get_all()

To find all VMs with the name "my first VM":

    vms = session.xenapi.VM.get_by_name_label('my first VM')

> **Note**
>
> Object `name_label` fields are not guaranteed to be unique and so the `get_by_name_label` API call returns a set of references rather than a single reference.

In addition to the methods of finding objects described above, most objects also contain references to other objects within fields. For example it is possible to find the set of VMs running on a particular host by calling:

    vms = session.xenapi.host.get_resident_VMs(host)

### Invoking synchronous operations on objects

Once object references have been acquired, operations may be invoked on them. For example to start a VM:

    session.xenapi.VM.start(vm, False, False)

All API calls are by default synchronous and will not return until the operation has completed or failed. For example in the case of `VM.start` the call does not return until the VM has started booting.

> **Note**
>
> When the `VM.start` call returns the VM will be booting. To determine when the booting has finished, wait for the in-guest agent to report internal statistics through the `VM_guest_metrics` object.

### Using Tasks to manage asynchronous operations

To simplify managing operations which take quite a long time (e.g. `VM.clone` and `VM.copy`) functions are available in two forms: synchronous (the default) and asynchronous. Each asynchronous function returns a reference to a task object which contains information about the in-progress operation including:

-   whether it is pending

-   whether it is has succeeded or failed

-   progress (in the range 0-1)

-   the result or error code returned by the operation

An application which wanted to track the progress of a `VM.clone` operation and display a progress bar would have code like the following:

    vm = session.xenapi.VM.get_by_name_label('my vm')
    task = session.xenapi.Async.VM.clone(vm)
    while session.xenapi.task.get_status(task) == "pending":
            progress = session.xenapi.task.get_progress(task)
            update_progress_bar(progress)
            time.sleep(1)
    session.xenapi.task.destroy(task)

> **Note**
>
> Note that a well-behaved client should remember to delete tasks created by asynchronous operations when it has finished reading the result or error. If the number of tasks exceeds a built-in threshold then the server will delete the oldest of the completed tasks.

### Subscribing to and listening for events

With the exception of the task and metrics classes, whenever an object is modified the server generates an event. Clients can subscribe to this event stream on a per-class basis and receive updates rather than resorting to frequent polling. Events come in three types:

-   `add` - generated when an object has been created;

-   `del` - generated immediately before an object is destroyed; and

-   `mod` - generated when an object's field has changed.

Events also contain a monotonically increasing ID, the name of the class of object and a snapshot of the object state equivalent to the result of a `get_record()`.

Clients register for events by calling `event.register()` with a list of class names or the special string "\*". Clients receive events by executing `event.next()` which blocks until events are available and returns the new events.

> **Note**
>
> Since the queue of generated events on the server is of finite length a very slow client might fail to read the events fast enough; if this happens an `EVENTS_LOST` error is returned. Clients should be prepared to handle this by re-registering for events and checking that the condition they are waiting for hasn't become true while they were unregistered.

The following python code fragment demonstrates how to print a summary of every event generated by a system: (similar code exists in `Xenserver-SDK/XenServerPython/samples/watch-all-events.py`)

    fmt = "%8s  %20s  %5s  %s"
    session.xenapi.event.register(["*"])
    while True:
        try:
            for event in session.xenapi.event.next():
                name = "(unknown)"
                if "snapshot" in event.keys():
                    snapshot = event["snapshot"]
                    if "name_label" in snapshot.keys():
                        name = snapshot["name_label"]
                print fmt % (event['id'], event['class'], event['operation'], name)           
        except XenAPI.Failure, e:
            if e.details == [ "EVENTS_LOST" ]:
                print "Caught EVENTS_LOST; should reregister"

Language bindings
-----------------

### C

The SDK includes the source to the C language binding in the directory `XenServer-SDK/libxenserver/src` together with a Makefile which compiles the binding into a library. Every API object is associated with a header file which contains declarations for all that object's API functions; for example the type definitions and functions required to invoke VM operations are all contained in `xen_vm.h`.

**C binding dependencies**

<table>
<col width="29%" />
<col width="70%" />
<tbody>
<tr class="odd">
<td align="left"><p>Platform supported:</p></td>
<td align="left"><p>Linux</p></td>
</tr>
<tr class="even">
<td align="left"><p>Library:</p></td>
<td align="left"><p>The language binding is generated as a <code>libxenserver.so</code> that is linked by C programs.</p></td>
</tr>
<tr class="odd">
<td align="left"><p>Dependencies:</p></td>
<td align="left"><ul>
<li><p>XML library (libxml2.so on GNU Linux)</p></li>
<li><p>Curl library (libcurl2.so)</p></li>
</ul></td>
</tr>
</tbody>
</table>

The following simple examples are included with the C bindings:

-   `test_vm_async_migrate`: demonstrates how to use asynchronous API calls to migrate running VMs from a slave host to the pool master.

-   `test_vm_ops`: demonstrates how to query the capabilities of a host, create a VM, attach a fresh blank disk image to the VM and then perform various powercycle operations;

-   `test_failures`: demonstrates how to translate error strings into enum\_xen\_api\_failure, and vice versa;

-   `test_event_handling`: demonstrates how to listen for events on a connection.

-   `test_enumerate`: demonstrates how to enumerate the various API objects.

### C&#35;

The C\# bindings are contained within the directory `XenServer-SDK/XenServer.NET` and include project files suitable for building under Microsoft Visual Studio. Every API object is associated with one C\# file; for example the functions implementing the VM operations are contained within the file `VM.cs`.

**C\# binding dependencies**

<table>
<col width="28%" />
<col width="68%" />
<tbody>
<tr class="odd">
<td align="left">Platform supported:</td>
<td align="left">Windows with .NET version 4.5</td>
</tr>
<tr class="even">
<td align="left">Library:</td>
<td align="left">The language binding is generated as a Dynamic Link Library <code>XenServer.dll</code> that is linked by C# programs.</td>
</tr>
<tr class="odd">
<td align="left">Dependencies:</td>
<td align="left"><code>CookComputing.XMLRpcV2.dll</code> is needed for the XenServer.dll to be able to communicate with the xml-rpc server. We test with version 2.1.0.6 and recommend that you use this version, though others may work.</td>
</tr>
</tbody>
</table>

Three examples are included with the C\# bindings in the directory `XenServer-SDK/XenServer.NET/samples` as separate projects of the `XenSdkSample.sln` solution:

-   `GetVariousRecords`: logs into a XenServer Host and displays information about hosts, storage and virtual machines;

-   `GetVmRecords`: logs into a XenServer Host and lists all the VM records;

-   `VmPowerStates`: logs into a XenServer Host, finds a VM and takes it through the various power states. Requires a shut-down VM to be already installed.

### Java

The Java bindings are contained within the directory `XenServer-SDK/XenServerJava` and include project files suitable for building under Microsoft Visual Studio. Every API object is associated with one Java file; for example the functions implementing the VM operations are contained within the file `VM.java`.

**Java binding dependencies**

<table>
<col width="29%" />
<col width="70%" />
<tbody>
<tr class="odd">
<td align="left"><p>Platform supported:</p></td>
<td align="left"><p>Linux and Windows</p></td>
</tr>
<tr class="even">
<td align="left"><p>Library:</p></td>
<td align="left"><p>The language binding is generated as a Java Archive file <code>xenserver-PRODUCT_VERSION.jar</code> that is linked by Java programs.</p></td>
</tr>
<tr class="odd">
<td align="left"><p>Dependencies:</p></td>
<td align="left"><ul>
<li>xmlrpc-client-3.1.jar is needed for the xenserver.jar to be able to communicate with the xml-rpc server.</li>
<li>ws-commons-util-1.0.2.jar is needed to run the examples.</li>
</ul></td>
</tr>
</tbody>
</table>

Running the main file `XenServer-SDK/XenServerJava/samples/RunTests.java` will run a series of examples included in the same directory:

-   `AddNetwork`: Adds a new internal network not attached to any NICs;

-   `SessionReuse`: Demonstrates how a Session object can be shared between multiple Connections;

-   `AsyncVMCreate`: Makes asynchronously a new VM from a built-in template, starts and stops it;

-   `VdiAndSrOps`: Performs various SR and VDI tests, including creating a dummy SR;

-   `CreateVM`: Creates a VM on the default SR with a network and DVD drive;

-   `DeprecatedMethod`: Tests a warning is displayed wehn a deprecated API method is called;

-   `GetAllRecordsOfAllTypes`: Retrieves all the records for all types of objects;

-   `SharedStorage`: Creates a shared NFS SR;

-   `StartAllVMs`: Connects to a host and tries to start each VM on it.

### PowerShell

The PowerShell bindings are contained within the directory `XenServer-SDK/XenServerPowerShell`. We provide the PowerShell module `XenServerPSModule` and source code exposing the XenServer API as Windows PowerShell cmdlets.

**PowerShell binding dependencies**

<table>
<col width="28%" />
<col width="68%" />
<tbody>
<tr class="odd">
<td align="left">Platform supported:</td>
<td align="left">Windows with .NET Framework 4.5 and PowerShell v4.0</td>
</tr>
<tr class="even">
<td align="left">Library:</td>
<td align="left"><code>XenServerPSModule</code></td>
</tr>
<tr class="odd">
<td align="left">Dependencies:</td>
<td align="left"><code>CookComputing.XMLRpcV2.dll</code> is needed to be able to communicate with the xml-rpc server. We test with version 2.1.0.6 and recommend that you use this version, though others may work.</td>
</tr>
</tbody>
</table>

These example scripts are included with the PowerShell bindings in the directory `XenServer-SDK/XenServerPowerShell/samples`:

-   `AutomatedTestCore.ps1`: demonstrates how to log into a XenServer host, create a storage repository and a VM, and then perform various powercycle operations;

-   `HttpTest.ps1`: demonstrates how to log into a XenServer host, create a VM, and then perform operations such as VM importing and exporting, patch upload, and retrieval of performance statistics.

### Python

The python bindings are contained within a single file: `XenServer-SDK/XenServerPython/XenAPI.py`.

**Python binding dependencies**

|:--|:--|
|Platform supported:|Linux|
|Library:|XenAPI.py|
|Dependencies:|None|

The SDK includes 7 python examples:

-   `fixpbds.py` - reconfigures the settings used to access shared storage;

-   `install.py` - installs a Debian VM, connects it to a network, starts it up and waits for it to report its IP address;

-   `license.py` - uploads a fresh license to a XenServer Host;

-   `permute.py` - selects a set of VMs and uses XenMotion to move them simultaneously between hosts;

-   `powercycle.py` - selects a set of VMs and powercycles them;

-   `shell.py` - a simple interactive shell for testing;

-   `vm_start_async.py` - demonstrates how to invoke operations asynchronously;

-   `watch-all-events.py` - registers for all events and prints details when they occur.

### Command Line Interface (CLI)

Besides using raw XML-RPC or one of the supplied language bindings, third-party software developers may integrate with XenServer Hosts by using the XE command line interface `xe`. The xe CLI is installed by default on XenServer hosts; a stand-alone remote CLI is also available for Linux. On Windows, the `xe.exe` CLI executable is installed along with XenCenter.

**CLI dependencies**

|:--|:--|
|Platform supported:|Linux and Windows|
|Library:|None|
|Binary:|xe (xe.exe on Windows)|
|Dependencies:|None|

The CLI allows almost every API call to be directly invoked from a script or other program, silently taking care of the required session management.
The XE CLI syntax and capabilities are described in detail in the [XenServer Administrator's Guide](https://docs.citrix.com/en-us/citrix-hypervisor/command-line-interface.html). For additional resources and examples, visit the [Citrix Knowledge Center](http://support.citrix.com).

> **Note**
>
> When running the CLI from a XenServer Host console, tab-completion of both command names and arguments is available.

Complete application examples
-----------------------------

This section describes two complete examples of real programs using the API.

### Simultaneously migrating VMs using XenMotion

This python example (contained in `XenServer-SDK/XenServerPython/samples/permute.py`) demonstrates how to use XenMotion to move VMs simultaneously between hosts in a Resource Pool. The example makes use of asynchronous API calls and shows how to wait for a set of tasks to complete.

The program begins with some standard boilerplate and imports the API bindings module

    import sys, time
    import XenAPI

Next the commandline arguments containing a server URL, username, password and a number of iterations are parsed. The username and password are used to establish a session which is passed to the function `main`, which is called multiple times in a loop. Note the use of `try: finally:` to make sure the program logs out of its session at the end.

    if __name__ == "__main__":
        if len(sys.argv) <> 5:
            print "Usage:"
            print sys.argv[0], " <url> <username> <password> <iterations>"
            sys.exit(1)
        url = sys.argv[1]
        username = sys.argv[2]
        password = sys.argv[3]
        iterations = int(sys.argv[4])
        # First acquire a valid session by logging in:
        session = XenAPI.Session(url)
        session.xenapi.login_with_password(username, password, "2.3",
                                           "Example migration-demo v0.1")
        try:
            for i in range(iterations):
                main(session, i)
        finally:
            session.xenapi.session.logout()

The `main` function examines each running VM in the system, taking care to filter out *control domains* (which are part of the system and not controllable by the user). A list of running VMs and their current hosts is constructed.

    def main(session, iteration):
        # Find a non-template VM object
        all = session.xenapi.VM.get_all()
        vms = []
        hosts = []
        for vm in all:
            record = session.xenapi.VM.get_record(vm)
            if not(record["is_a_template"]) and \
               not(record["is_control_domain"]) and \
               record["power_state"] == "Running":
                vms.append(vm)
                hosts.append(record["resident_on"])
        print "%d: Found %d suitable running VMs" % (iteration, len(vms))

Next the list of hosts is rotated:

    # use a rotation as a permutation
        hosts = [hosts[-1]] + hosts[:(len(hosts)-1)]

Each VM is then moved using XenMotion to the new host under this rotation (i.e. a VM running on host at position 2 in the list will be moved to the host at position 1 in the list etc.) In order to execute each of the movements in parallel, the asynchronous version of the `VM.pool_migrate` is used and a list of task references constructed. Note the `live` flag passed to the `VM.pool_migrate`; this causes the VMs to be moved while they are still running.

    tasks = []
        for i in range(0, len(vms)):
            vm = vms[i]
            host = hosts[i]
            task = session.xenapi.Async.VM.pool_migrate(vm, host, { "live": "true" })
            tasks.append(task)

The list of tasks is then polled for completion:

    finished = False
        records = {}
        while not(finished):
            finished = True
            for task in tasks:
                record = session.xenapi.task.get_record(task)
                records[task] = record
                if record["status"] == "pending":
                    finished = False
            time.sleep(1)

Once all tasks have left the *pending* state (i.e. they have successfully completed, failed or been cancelled) the tasks are polled once more to see if they all succeeded:

    allok = True
        for task in tasks:
            record = records[task]
            if record["status"] <> "success":
                allok = False

If any one of the tasks failed then details are printed, an exception is raised and the task objects left around for further inspection. If all tasks succeeded then the task objects are destroyed and the function returns.

    if not(allok):
            print "One of the tasks didn't succeed at", \
                time.strftime("%F:%HT%M:%SZ", time.gmtime())
            idx = 0
            for task in tasks:
                record = records[task]
                vm_name = session.xenapi.VM.get_name_label(vms[idx])
                host_name = session.xenapi.host.get_name_label(hosts[idx])
                print "%s : %12s %s -> %s [ status: %s; result = %s; error = %s ]" % \
                      (record["uuid"], record["name_label"], vm_name, host_name,      \
                       record["status"], record["result"], repr(record["error_info"]))
                idx = idx + 1
            raise "Task failed"
        else:
            for task in tasks:
                session.xenapi.task.destroy(task)

### Cloning a VM using the XE CLI

This example is a `bash` script which uses the XE CLI to clone a VM taking care to shut it down first if it is powered on.

The example begins with some boilerplate which first checks if the environment variable `XE` has been set: if it has it assumes that it points to the full path of the CLI, else it is assumed that the XE CLI is on the current path. Next the script prompts the user for a server name, username and password:

    # Allow the path to the 'xe' binary to be overridden by the XE environment variable
    if [ -z "${XE}" ]; then
      XE=xe
    fi

    if [ ! -e "${HOME}/.xe" ]; then
      read -p "Server name: " SERVER
      read -p "Username: " USERNAME
      read -p "Password: " PASSWORD
      XE="${XE} -s ${SERVER} -u ${USERNAME} -pw ${PASSWORD}"
    fi

Next the script checks its commandline arguments. It requires exactly one: the UUID of the VM which is to be cloned:

    # Check if there's a VM by the uuid specified
    ${XE} vm-list params=uuid | grep -q " ${vmuuid}$"
    if [ $? -ne 0 ]; then
            echo "error: no vm uuid \"${vmuuid}\" found"
            exit 2
    fi

The script then checks the power state of the VM and if it is running, it attempts a clean shutdown. The event system is used to wait for the VM to enter state "Halted".

> **Note**
>
> The XE CLI supports a command-line argument `--minimal` which causes it to print its output without excess whitespace or formatting, ideal for use from scripts. If multiple values are returned they are comma-separated.

    # Check the power state of the vm
    name=$(${XE} vm-list uuid=${vmuuid} params=name-label --minimal)
    state=$(${XE} vm-list uuid=${vmuuid} params=power-state --minimal)
    wasrunning=0

    # If the VM state is running, we shutdown the vm first
    if [ "${state}" = "running" ]; then
            ${XE} vm-shutdown uuid=${vmuuid}
            ${XE} event-wait class=vm power-state=halted uuid=${vmuuid}
            wasrunning=1
    fi

The VM is then cloned and the new VM has its `name_label` set to `cloned_vm`.

    # Clone the VM
    newuuid=$(${XE} vm-clone uuid=${vmuuid} new-name-label=cloned_vm)

Finally, if the original VM had been running and was shutdown, both it and the new VM are started.

    # If the VM state was running before cloning, we start it again
    # along with the new VM.
    if [ "$wasrunning" -eq 1 ]; then
            ${XE} vm-start uuid=${vmuuid}
            ${XE} vm-start uuid=${newuuid}
    fi
