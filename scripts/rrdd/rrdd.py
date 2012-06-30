#============================================================================
# This library is free software; you can redistribute it and/or
# modify it under the terms of version 2.1 of the GNU Lesser General Public
# License as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#============================================================================
# Copyright (C) 2006-2007 XenSource Inc.
#============================================================================
#
# Parts of this file are based upon xmlrpclib.py, the XML-RPC client
# interface included in the Python distribution.
#
# Copyright (c) 1999-2002 by Secret Labs AB
# Copyright (c) 1999-2002 by Fredrik Lundh
#
# By obtaining, using, and/or copying this software and/or its
# associated documentation, you agree that you have read, understood,
# and will comply with the following terms and conditions:
#
# Permission to use, copy, modify, and distribute this software and
# its associated documentation for any purpose and without fee is
# hereby granted, provided that the above copyright notice appears in
# all copies, and that both that copyright notice and this permission
# notice appear in supporting documentation, and that the name of
# Secret Labs AB or the author not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# SECRET LABS AB AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
# TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANT-
# ABILITY AND FITNESS.  IN NO EVENT SHALL SECRET LABS AB OR THE AUTHOR
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.
# --------------------------------------------------------------------

import httplib
import md5
import os
import simplejson as json
import socket
import sys
import time
import xmlrpclib

class Failure(Exception):
  def __init__(self, details):
    self.details = details

  def __str__(self):
    try:
      return str(self.details)
    except Exception, exn:
      print >>sys.stderr, exn
      return "rrdd failure: %s" % str(self.details)

  def _details_map(self):
    return dict([(str(i), self.details[i])
      for i in range(len(self.details))])

class UDSHTTPConnection(httplib.HTTPConnection):
  """HTTPConnection subclass to allow HTTP over Unix domain sockets."""
  def connect(self):
    path = self.host.replace("_", "/")
    self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    self.sock.connect(path)

class UDSHTTP(httplib.HTTP):
  _connection_class = UDSHTTPConnection

class UDSTransport(xmlrpclib.Transport):
  def __init__(self, use_datetime = 0):
    self._use_datetime = use_datetime
    self._extra_headers = []

  def add_extra_header(self, key, value):
    self._extra_headers += [(key, value)]

  def make_connection(self, host):
    # Python 2.4 compatibility
    if sys.version_info[0] <= 2 and sys.version_info[1] < 6:
      return UDSHTTP(host)
    else:
      return UDSHTTPConnection(host)

  def send_request(self, connection, handler, request_body):
    connection.putrequest("POST", handler)
    for key, value in self._extra_headers:
      connection.putheader(key, value)

class Proxy(xmlrpclib.ServerProxy):
  def __init__(self, uri, transport=None, encoding=None, verbose=0,
         allow_none=1):
    xmlrpclib.ServerProxy.__init__(self, uri, transport, encoding,
                     verbose, allow_none)
    self.transport = transport

  def request(self, methodname, params):
    if len(params) == 0: params = (None,)
    return self.parse_result(getattr(self, methodname)(*params))

  def parse_result(self, result):
    if type(result) != dict or 'Status' not in result:
      raise xmlrpclib.Fault(500, 'Missing Status in response from server' + result)
    if result['Status'] == 'Success':
      if 'Value' in result:
        return result['Value']
      else:
        raise xmlrpclib.Fault(500, 'Missing Value in response from server')
    else:
      if 'ErrorDescription' in result:
        raise Failure(result['ErrorDescription'])
      else:
        raise xmlrpclib.Fault(
          500, 'Missing ErrorDescription in response from server')

class Dispatcher:
  def __init__(self, send, name):
    self.send = send
    self.name = name

  def __repr__(self):
    if self.name:
      return '<rrdd.Dispatcher for %s>' % self.name
    else:
      return '<rrdd.Dispatcher>'

  def __getattr__(self, name):
    if self.name is None:
      return Dispatcher(self.send, name)
    else:
      return Dispatcher(self.send, "%s.%s" % (self.name, name))

  def __call__(self, *args):
    return self.send(self.name, args)

class DataSource:
  class Type:
    ABSOLUTE = "absolute"
    RATE = "rate"
    ABSOLUTE_TO_RATE = "absolute_to_rate"
  class ValueType:
    FLOAT = "float"
    INT64 = "int64"

  def __init__(self, name, value, description = "", units = "",
               type = Type.ABSOLUTE, value_type = ValueType.FLOAT,
               min = "-infinity", max = "infinity"):
    self.name = name
    self.value = value
    self.description = description
    self.units = units
    self.type = type
    self.value_type = value_type
    self.min = min
    self.max = max
    if min != float("-infinity"): self.min = str(float(min))
    if max != float("infinity"): self.max = str(float(max))

  def to_dict(self):
    return {self.name : {"value": self.value,
            "description": self.description, "units": self.units,
            "value_type": self.value_type, "min": self.min, "max": self.max}}

class API:
  def __init__(self, uid, frequency = "Five_Seconds"):
    self.uid = uid
    self.frequency = frequency
    self.frequency_in_seconds = self.frequency_to_seconds(frequency)
    p = Proxy("http://_var_xapi_xcp-rrdd/", transport=UDSTransport())
    self.dispatcher = Dispatcher(p.request, None).Plugin
    self.header = self.dispatcher.get_header()
    self.path = self.dispatcher.get_path({"uid": uid})
    base_path = os.path.dirname(self.path)
    if not os.path.exists(base_path): os.makedirs(base_path)
    self.file = open(self.path, "w")

  def __del__(self):
    self.deregister()

  def frequency_to_seconds(self, frequency):
    if frequency == "Five_Seconds": return 5
    raise NameError(frequency)

  def get_header(self):
    return self.header

  def get_path(self):
    return self.path

  def register(self):
    params = {"uid": self.uid, "frequency": self.frequency}
    return self.dispatcher.register(params)

  def deregister(self):
    return self.dispatcher.deregister({"uid": self.uid})

  def next_reading(self):
    return self.dispatcher.next_reading({"uid": self.uid})

  def update_and_sleep(self, dss):
    timestamp = int(time.time())
    combined = dict()
    for ds in dss: combined = dict(combined.items() + ds.to_dict().items())
    format = {"timestamp": timestamp, "datasources": combined}
    s = json.dumps(format, sort_keys=True, indent=2)
    payload = '\n'.join([l.rstrip() for l in s.splitlines()])
    m = md5.new()
    m.update(payload)
    output = ""
    output += self.header
    output += "%08X\n" % len(payload)
    output += m.hexdigest() + "\n"
    output += payload
    self.file.seek(0)
    self.file.write(output)
    self.file.flush()
    try: self.register() # this ensures the file is read even if rrdd restarts
    except: pass # rrdd appears to be down
    time.sleep(self.frequency_in_seconds)
