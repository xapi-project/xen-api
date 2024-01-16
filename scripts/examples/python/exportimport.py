#!/usr/bin/env python3

# Copyright (c) 2014 Citrix, Inc.
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Demonstrate how to
#  - export raw disk images
#  - import raw disk images
#  - connect an export to an import to copy a raw disk image


import sys, os, socket, urllib.request, urllib.error, urllib.parse, XenAPI, traceback, ssl, time

def exportimport(url, xapi, session, src_vdi, dst_vdi):
  # If an HTTP operation fails then it will record the error on the task
  # object. Note you can't use the HTTP response code for this because
  # it must be sent *before* the stream is processed.
  import_task = xapi.xenapi.task.create("import " + dst_vdi, "")
  export_task = xapi.xenapi.task.create("export " + src_vdi, "")
  try:
    # an HTTP GET of this will export a disk:
    get_url = "/export_raw_vdi?session_id=%s&vdi=%s&task_id=%s" % (session, src_vdi, export_task)
    # an HTTP PUT to this will import a disk:
    put_url = "/import_raw_vdi?session_id=%s&vdi=%s&task_id=%s" % (session, dst_vdi, import_task)

    # 'data' is the stream of raw data:
    data = urllib.request.urlopen(url + get_url)

    # python's builtin library doesn't support HTTP PUT very well
    # so we do it manually. Note xapi doesn't support Transfer-encoding:
    # chunked so we must send the data raw.
    url = urllib.parse.urlparse(url)
    host = url.netloc.split(":")[0] # assume port 443
    if url.scheme != "https":
      print("Sorry, this example only supports HTTPS (not HTTP)", file=sys.stderr)
      print("Plaintext HTTP has the following problems:", file=sys.stderr)
      print("  - the data can be captured by other programs on the network", file=sys.stderr)
      print("  - some network middleboxes will mangle the data", file=sys.stderr)
      # time wasted debugging a problem caused by a middlebox: 3hrs
      # Just use HTTPS!
      return

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    output = ssl.wrap_socket(s)
    output.connect((host, 443))

    # HTTP/1.0 with no transfer-encoding
    headers = [
      "PUT %s HTTP/1.0" % put_url,
      "Connection:close",
      ""
    ]
    print("Sending HTTP request:")
    for h in headers:
      output.send((h + "\r\n").encode())
      print("%s\r\n" % h)
    result = output.recv(1024)
    print("Received HTTP response:")
    print(result)
    if "200 OK" not in result:
      print("Expected an HTTP 200, got %s" % result, file=sys.stderr)
      return

    # Copy the raw bytes, signal completion by closing the socket
    virtual_size = int(xapi.xenapi.VDI.get_virtual_size(src_vdi))
    print("Copying %Ld bytes" % virtual_size)
    left = virtual_size
    while left > 0:
      block = data.read(min(65536, left))
      if block is None:
        break
      output.send(block)
      left = left - len(block)
    output.close()

    # Wait for the tasks to complete and check whether they both
    # succeeded. It takes a few seconds to detach the disk etc.
    finished = False
    while not finished:
      import_status = xapi.xenapi.task.get_status(import_task)
      export_status = xapi.xenapi.task.get_status(export_task)
      finished = import_status != "pending" and export_task != "pending"
      time.sleep(1)
    if import_status == "success" and export_status == "success":
      print("OK")
    else:
      print("FAILED")
      if import_status != "success":
        print("The import task failed with: ", " ".join(xapi.xenapi.task.get_error_info(import_task)))
      if export_status != "success":
        print("The export task failed with: ", " ".join(xapi.xenapi.task.get_error_info(export_task)))

  finally:
    # The task creator has to destroy them at the end:
    xapi.xenapi.task.destroy(import_task)
    xapi.xenapi.task.destroy(export_task)

if __name__ == "__main__":
  if len(sys.argv) != 5:
    print("Usage:")
    print(sys.argv[0], " <url> <username> <password> <vdi-uuid>")
    print(" -- creates a fresh VDI and streams the contents of <vdi-uuid> into it.")
    print()
    print("Example:")
    print("SR=$(xe pool-list params=default-SR --minimal)")
    print("VDI=$(xe vdi-create sr-uuid=$SR name-label=test virtual-size=128MiB type=user)")
    print(sys.argv[0], "https://localhost password $VDI")
    sys.exit(1)
  url = sys.argv[1]
  username = sys.argv[2]
  password = sys.argv[3]
  vdi_uuid = sys.argv[4]
  # First acquire a valid session by logging in:
  xapi = XenAPI.Session(url)
  xapi.xenapi.login_with_password(username, password, '1.0', 'xen-api-scripts-exportimport.py')
  dst_vdi = None
  try:
    src_vdi = xapi.xenapi.VDI.get_by_uuid(vdi_uuid)
    sr = xapi.xenapi.VDI.get_SR(src_vdi)
    # Create an empty VDI with the same initial parameters (e.g. size)
    # to upload into
    vdi_args = xapi.xenapi.VDI.get_record(src_vdi)
    dst_vdi = xapi.xenapi.VDI.create(vdi_args)
    exportimport(url, xapi, xapi._session, src_vdi, dst_vdi)
  except Exception as e:
    print("Caught %s: trying to clean up" % str(e))
    traceback.print_exc()
    if dst_vdi:
      xapi.xenapi.VDI.destroy(dst_vdi)
  finally:
    xapi.xenapi.logout()
