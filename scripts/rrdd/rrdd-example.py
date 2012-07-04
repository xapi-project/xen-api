#!/usr/bin/env python

import rrdd, os

if __name__ == "__main__":
  # Create a proxy for communicating with xcp-rrdd.
  api = rrdd.API(plugin_id = "host_mem")
  while True:
    # Wait until 0.5 seconds before xcp-rrdd is going to read the output file.
    api.wait_until_next_reading(neg_shift = .5)
    # Collect measurements.
    cmd = "free -k | grep Mem | awk '{print $2, $3, $4}'"
    vs = os.popen(cmd).read().strip().split()
    # Tell the proxy which datasources should be exposed in this iteration.
    api.set_datasource("used_mem", vs[1], min_val = 0, max_val = vs[0])
    api.set_datasource("free_mem", vs[2], min_val = 0, max_val = vs[0])
    # Write all required information into a file about to be read by xcp-rrdd.
    api.update()
