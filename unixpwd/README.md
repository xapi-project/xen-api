
# Unix Password

This is a simple C library with OCaml bindings to support updating Unix
account passwords in /etc/passwd and /etc/shadow. It is not meant to
support other operations, like creating or deleting accounts. For this
you might be interested in [opasswd] which exposed the complete C API
for password handling to [OCaml].

Be aware that updating password entries requires root privileges. So
update functions will fail unless called with root privileges.

# OCaml Code

The [OCaml] code lives in directory `src/` and provides simple bindings
for the underlying C functions.

# C Code

The C code lives in directory `c/` and contains a Makefile that is not
used by the build process for [OCaml] bindings but was used during
development of the C code.

# Command Line Interface

The Makefile in directory `c/` builds a small binary for tests. This is
especially useful for debugging purposes with GDB.

```sh
$ cd c
$ make
$ ./unixpwd root
root: x
/etc/passwd: root: x
can't find shadow entry for root: Permission denied

$ ./unixpwd | head
root:x:0:0:root:/root
daemon:x:1:1:daemon:/usr/sbin
bin:x:2:2:bin:/bin
sys:x:3:3:sys:/dev
sync:x:4:65534:sync:/bin
games:x:5:60:games:/usr/games
man:x:6:12:man:/var/cache/man
lp:x:7:7:lp:/var/spool/lpd
mail:x:8:8:mail:/var/mail
news:x:9:9:news:/var/spool/news
```

# Testing

Directory `test/` contains a small [OCaml] program that reads and
updates password entries. Since it needs to be run as root and uses the
system's password database, it is not run automatically. It is
configures to update the account `unixpwd` that you might want to create
for this purpose.

# Known Issues

The Makefile in the `c/` directory contains support for running
[valgrind] to find memory leaks. Valgrind reports some reachable memory
at exit. As far as I can tell, this is related to memory allocated by
libc for `/etc/nsswitch`.

* [getpwnam_r memory leak](https://stackoverflow.com/q/1447018)

[OCaml]:   https://www.ocaml.org/
[opasswd]: https://github.com/xapi-project/ocaml-opasswd.git
[valgrind]: http://valgrind.org/
