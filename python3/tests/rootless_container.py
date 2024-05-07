"""rootless_container.py: Create a rootless container on any Linux and GitHub CI"""
import ctypes
import os

# Unshare the user namespace, so that the calling process is moved into a new
# user namespace which is not shared with any previously existing process.
# Needed so that the current user id can be mapped to 0 for getting a new
# mount namespace.
CLONE_NEWUSER = 0x10000000
# Unshare the mount namespace, so that the calling process has a private copy
# of its root directory namespace which is not shared with any other process:
CLONE_NEWNS = 0x00020000
# Flags for mount(2):
MS_BIND = 4096
MS_REC = 16384
MS_PRIVATE = 1 << 18


def unshare(flags):  # type:(int) -> None
    """Wrapper for the library call to unshare Linux kernel namespaces"""
    lib = ctypes.CDLL(None, use_errno=True)
    lib.unshare.argtypes = [ctypes.c_int]
    rc = lib.unshare(flags)
    if rc != 0:  # pragma: no cover
        errno = ctypes.get_errno()
        raise OSError(errno, os.strerror(errno), flags)


def mount(source="none", target="", fs="", flags=0, options=""):
    # type:(str, str, str, int, str) -> None
    """Wrapper for the library call mount(). Supports Python2.7 and Python3.x"""
    lib = ctypes.CDLL(None, use_errno=True)
    lib.mount.argtypes = (
        ctypes.c_char_p,
        ctypes.c_char_p,
        ctypes.c_char_p,
        ctypes.c_ulong,
        ctypes.c_char_p,
    )
    result = lib.mount(
        source.encode(), target.encode(), fs.encode(), flags, options.encode()
    )
    if result < 0:  # pragma: no cover
        errno = ctypes.get_errno()
        raise OSError(
            errno,
            "mount " + target + " (" + options + "): " + os.strerror(errno),
        )


def umount(target):  # type:(str) -> None
    """Wrapper for the Linux umount system call, supports Python2.7 and Python3.x"""
    lib = ctypes.CDLL(None, use_errno=True)
    result = lib.umount(ctypes.c_char_p(target.encode()))
    if result < 0:  # pragma: no cover
        errno = ctypes.get_errno()
        raise OSError(errno, "umount " + target + ": " + os.strerror(errno))


def enter_private_mount_namespace():
    """Enter a private mount and user namespace with the user and simulate uid 0

    Some code like mount() requires to be run as root. The container simulates
    root-like privileges and a new mount namespace that allows mount() in it.

    Implements the equivalent of `/usr/bin/unshare --map-root-user --mount`
    """

    # Read the actual user and group ids before entering the new user namespace:
    real_uid = os.getuid()
    real_gid = os.getgid()
    unshare(CLONE_NEWUSER | CLONE_NEWNS)
    # Setup user map to map the user id to behave like uid 0:
    with open("/proc/self/uid_map", "wb") as proc_self_user_map:
        proc_self_user_map.write(b"0 %d 1" % real_uid)
    with open("/proc/self/setgroups", "wb") as proc_self_set_groups:
        proc_self_set_groups.write(b"deny")
    # Setup group map for the user's gid to behave like gid 0:
    with open("/proc/self/gid_map", "wb") as proc_self_group_map:
        proc_self_group_map.write(b"0 %d 1" % real_gid)
    # Private root mount in the mount namespace top support mounting a private tmpfs:
    mount(target="/", flags=MS_REC | MS_PRIVATE)
    return True
