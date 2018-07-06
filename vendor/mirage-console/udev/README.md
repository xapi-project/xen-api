This shows how to customise the manifestation of a Xen console device in Linux.

Install the udev rule:
```
cp 99-xen-console.rules /etc/udev/rules.d/
cp xenconsole-setup-tty /lib/udev
```

Attach a console with a name:
```
mirage-console connect trusty --name myspecialname
```

Observe the console has been created in /dev/xenconsole/myspecialname
