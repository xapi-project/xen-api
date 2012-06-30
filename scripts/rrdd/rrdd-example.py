#!/usr/bin/env python

import rrdd, os

if __name__ == "__main__":
  api = rrdd.API("host_mem")
  while True:
    cmd = "free -k | grep Mem | awk '{print $2, $3, $4}'"
    values = os.popen(cmd).read().strip().split()
    total = float(values[0])
    used = float(values[1])
    free = float(values[2])
    ds_used = rrdd.DataSource("used_mem", used, min = 0, max = total)
    ds_free = rrdd.DataSource("free_mem", free, min = 0, max = total)
    api.update_and_sleep([ds_used, ds_free])
