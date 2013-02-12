squeezed: a xen host memory ballooning daemon
---------------------------------------------

Squeezed uses [ballooning](http://static.usenix.org/events/osdi02/tech/full_papers/waldspurger/waldspurger_html/node6.html)
to move memory between running VMs. It is able to:

  1. avoid wasting host memory: unused memory can be gifted to VMs
  2. share memory according to a configured policy, so some VMs will use more than others
  3. "squeeze" existing VMs to make room to start new VMs.

Squeezed is an optional component of the [xapi toolstack](http://wiki.xen.org/wiki/Choice_of_Toolstacks).

building the design documentation
---------------------------------

  git clone git://github.com/xen-org/squeezed
  cd squeezed/doc
  make

building the development version
--------------------------------

First follow the [xenopsd build instructions](http://wiki.xen.org/wiki/Building_Xenopsd): this will ensure your environment is working correctly.

To build squeezed:

  git clone git://github.com/xen-org/squeezed
  cd squeezed
  obuild configure
  obuild build

