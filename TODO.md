
Cohttp support for basic auth (done)
Cohttp support for cookies (done)
Need to send and receive fds (done)
FD passing (done)
Config file parsing (done) (tested)
Daemonize helper (done)

Refactor qemu support across backends: we have support for 'upstream' (ie
normal) qemu in the qemu/ backend. We need to be able to use this in the
'classic' backend to construct ceph commandlines like:

/usr/lib/xen/bin/qemu-system-i386
  -xen-domid 18  (* alternative to -enable-kvm *)
  -m 1024
  -name ubuntu1204-ceph
  -vnc 127.0.0.1:0
  -k en-us
  -vga std
  -boot order=dc
  -usb
  -usbdevice tablet
  -net none
  -M xenfv
  -drive file=rbd:rbd/ubuntu1204.img,if=none,id=drive-ide0-0-1
  -device ide-hd,bus=ide.0,unit=1,drive=drive-ide0-0-1,id=ide0-0-1,bootindex=1
