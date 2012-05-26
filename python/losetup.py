
# Use Linux "losetup" to create block devices from files
class Losetup:
    # [_find dbg path] returns the loop device associated with [path]
    def _find(self, root, dbg, path):
        for line in run(dbg, "losetup -a").split("\n"):
            line = line.strip()
            if line <> "":
                bits = line.split()
                loop = bits[0][0:-1]
                this_path = bits[2][1:-1]
                if this_path == path:
                    return loop
        return None
    # [add dbg path] creates a new loop device for [path] and returns it
    def add(self, root, dbg, path):
        run(dbg, "losetup -f %s" % path)
        return self._find(root, dbg, path)
    # [remove dbg path] removes the loop device associated with [path]
    def remove(self, root, dbg, path):
        loop = self._find(root, dbg, path)
        run(dbg, "losetup -d %s" % loop)

