import util, errno

TAP_CTL="/usr/sbin/tap-ctl"

class Control:
    def __init__(self, minor = None, pid = None):
        if minor == None:
            minor = self._allocate()
        if pid == None:
            pid = self._spawn()
            self._attach(minor, pid)
        self.minor = str(minor)
        self.pid = str(pid)

    def _allocate(self):
        cmd = [TAP_CTL, "allocate"]
        return util.pread2(cmd)[len("/dev/xen/blktap-2/tapdev"):].strip()

    def _spawn(self):
        cmd = [TAP_CTL, "spawn"]
        line = util.pread2(cmd)
        return line.strip()

    def _attach(self, minor, pid):
        cmd = [TAP_CTL, "attach", "-m", minor, "-p", pid]
        util.pread2(cmd)

    def open(self, ty, path):
        cmd = [TAP_CTL, "open", "-m", self.minor, "-p", self.pid, "-a", "%s:%s" % (ty, path)]
        util.pread2(cmd)

    def close(self):
        cmd = [TAP_CTL, "close", "-m", self.minor, "-p", self.pid]
        util.pread2(cmd)

    def detach(self):
        cmd = [TAP_CTL, "detach", "-m", self.minor, "-p", self.pid]
        util.pread2(cmd)

    def free(self):
        cmd = [TAP_CTL, "free", "-m", self.minor]
        util.pread2(cmd)

def list():
    cmd = [TAP_CTL, "list"]
    output = util.ioretry(lambda: util.pread2(cmd), errlist = [errno.EPROTO, errno.ENOENT])
    results = []
    for line in output.split("\n"):
        # FIXME: tap-ctl writes error messages to stdout and
        # confuses this parser
        if line == "blktap kernel module not installed\n":
            # This isn't pretty but (a) neither is confusing stdout/stderr
            # and at least causes the error to describe the fix
            raise Exception, "blktap kernel module not installed: try 'modprobe blktap'"

        minor = None
        pid = None
        for field in line.rstrip().split(' ', 3):
            bits = field.split('=')
            if len(bits) == 2:
                key, val = field.split('=')

                if key == "pid":
                    pid = int(val, 10)
                elif key == "minor":
                    minor = int(val, 10)

            else:
                util.SMlog("Ignoring unexpected tap-ctl output: %s" % repr(field))
        if minor or pid:
            c = Control(minor = minor, pid = pid)
            results.append(c)
    return results
