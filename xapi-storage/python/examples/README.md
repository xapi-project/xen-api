# Simple, file based, SMAPIv3 SR #

The implementation of the various plugins comprises a number of python
scripts which implement different interfaces from
https://xapi-project.github.io/xapi-storage/?python#introduction. When
installed using the *install.sh* script a number of symlinks will be
created to each of the implementation files, one for each supported
command for the file. The dispatch code in the implementation scripts
will dispatch to matching methods within the implementation class for
the plugin. As far as possible the implementation has been kept
stateless and stores on disk only the volume data associated with the
SR, there are no external data files or databases used, either or both
of which may be desirable in fully fledged implementations.

All of the code runs as root in the control domain of the hypervisor,
this example has not been extensively security audited and any
production implementation should take care to avoid common security
issues such as path injection and ensure that data is only accessed
from within the boundaries of the storage associated with the SR.

## Plugins ##

### Volume plugin ###

Stores volume data in raw data files located in a directory on a
locally mounted filesystem.

#### plugin.py ####

Implements the plugin interface
https://xapi-project.github.io/xapi-storage/?python#plugin. This
declares the name of the plugin, version, vendor, copyright etc and
the capabilities supported by the implementation of the plugin. It
also declares what configuration options are required by the plugin
implementation, in this case the only configuration option is *path*
which defines the folder path where the SR will store it's volume data
files.

#### sr.py ####

Implements the SR interface of the volume plugin
https://xapi-project.github.io/xapi-storage/?python#volume-interface-sr. Only
the methods required for a minimally functional SR are
implemented. Notably *SR.probe* is not implemented but could have a
minimal implementation which would just check that the provided
filesystem path existed and was a directory. Additionally
*SR.set_name* and *SR.set_description* are non-functional in this
implementation as the name and description are only held within the SR
URI returned from the *SR.attach* operation, they could be stored for
instance in a JSON formatted file in the SR directory and then could
be updated.

*SR.probe* A production implementation of this interface should
support the *SR.probe* command so that it is possible to progressively
build up configuration options, where that makes sense, until all the
required options are defined and an SR can be created from the
configuration set. For instance something like iSCSI could start with
just the target address and port and then list the available target
IQNs. Having selected a target IQN the available LUNS could be listed
and then once a LUN is selected either confirm the LUN is empty or if
it already contains data consistent with it being an SR return the
details of the SR contained in the LUN to allow for reattaching a
currently detached SR.

The *SR.create* and *SR.attach* commands take a set of configuration
options either as individual *--configuration* parameters or as a JSON
dictionary. *SR.create* extends the provided dictionary with the
*uuid*, *name* and *description* parameters and returns this. The
extended dictionary set must be supplied to *SR.attach* for proper
operation, as noted above these extra parameters could be moved to a
file stored in the SR folder to avoid needing to do this. Other than
encoding its parameters into a URI, the *SR.attach* operation doesn't
need to do anything as it is assumed the SR storage is already
mounted, less trivial implementations might need to mount a filesystem
here.

*SR.detach* and *SR.destroy* are no-ops in this implementation as
 nothing is required to be done to satisfy these commands.

*SR.stat* will return a python dictionary representing the sr_stat
 struct defined
 https://xapi-project.github.io/xapi-storage/?python#volume-type-definitions. Most
 of the required data is unpacked from the SR URI.

*SR.set_name* and *SR.set_description* are defined but do nothing
here.

*SR.ls* lists all the volumes present on the SR and returns a list of
 volume structs from
 https://xapi-project.github.io/xapi-storage/?python#volume-type-definitions. 

#### volume.py ####

Implements the Volume interface of the volume plugin
https://xapi-project.github.io/xapi-storage/?python#volume-interface-volume. Only
the methods required to support the declared capabilities are defined.

*Volume.create* will create a new volume file of the requested size in
the SR directory. Multi-access, shareable volumes are not supported
in this implementation. Files are named using a uniquely created UUID
which is used as the volume *key* in future operations. The *name*,
*description* and *size* parameters need to be persisted and be
available in future operation without them being passed back in via
stateless URIs, so these are stored in a second file adjacent to the
volume file names *&lt;uuid&gt;.inf*. The operation returns a volume
struct as defined by
https://xapi-project.github.io/xapi-storage/?python#volume-type-definitions,
the URIs are an ordered list in prefence of use, in this case a single
URI is returned. The URI scheme selects the datapath plugin to use for
reading/writing to the volumes.

*Volume.destroy* deletes the two files associated with the provided
volume key. It should not be an error for the files to have already
been deleted but this implementation is not idempotent.

*Volume.set_name* and *Volume.set_description* do as their name
suggest and update the name and description values in the volume
*.inf* file.

*Volume.set* is not implemented here.

*Volume.resize* will grow a volume if the requested size is bigger
than the current volume size.

### Datapath plugin ###

The datapath plugin is selected by the URI scheme of the volume from
the SR, in this case the scheme is *loop+blkback*. Access to the file
is achieved by creating a kernel loopback device for the file, using
*losetup*. The datapath configuration the plugin returns select the
*vbd* backend_type which will use the kernel *xen-blkback* driver.

#### plugin.py ####

Implements the plugin interface
https://xapi-project.github.io/xapi-storage/?python#plugin. This
declares the name of the plugin, version, vendor, copyright etc and
the capabilities supported by the implementation of the
plugin. Simpler than the definition for a volume plugin as no features
are exposed.

#### datapath.py ####

*Datapath.open* no specific operations are performed when opening the
 datapath.
 
*Datapath.attach* opens a loopback block device for the file
referenced in the URI, optionally applying a sizelimit defined by a
size query parameter in the URI. Two implementations are returned
from the attach operation
  * BlockDevice  
    This declares a block device for block level access from the
    hypervisor control domain, typically used as a boot device or when
    paravirtualised devices drivers are not available in the VM.
  * XenDisk  
    This declares the configuration data for a paravirtualised
    datapath, in this case the kernel *xen-blkback* driver select by
    the *backend_type* being set to *vbd*. Additional parameters for
    the backend driver are included in the *params* and *extra*
    entries in the dictionary.

*Datapath.activate* no specific operations are required to activate
this datapath.

*Datapath.deactivate* no specific operations are required to
deactivate this datapath.

*Datapath.detach* finds the loop device for the provided URI file and
detaches the device, effectively closing it.
 
*Datapath.close* no specific operations are required to close the
datapath. 

## Limitations ##

  * No support for volume snapshots or cloning
    * As the data is stored in raw files
    * Using a filesystem with reflinks, e.g. btrfs, could allow this to
      be implemented relatively simply
  * No locking
    * Relies on all volumes being independent entities
  * No check for existing active datapath when attaching, this could
    be done using local system storage to store the state of the file
