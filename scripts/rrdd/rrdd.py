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
  def __init__(self, uri, transport = None, encoding = None, verbose = 0,
         allow_none = 1):
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

  def __init__(self, name, value, description, units, ty, value_ty, min_val,
               max_val):
    self.name = name
    if value_ty == DataSource.ValueType.FLOAT:
      self.value = float(value)
    elif value_ty == DataSource.ValueType.INT64:
      self.value = long(value)
    else: raise NotImplementedError
    self.description = description
    self.units = units
    self.ty = ty
    self.value_ty = value_ty
    self.min_val = float(min_val)
    self.max_val = float(max_val)
    if min_val != float("-infinity"): self.min_val = str(float(min_val))
    if max_val != float("infinity"): self.max_val = str(float(max_val))

  def to_dict(self):
    return {self.name : {"value": self.value,
            "description": self.description, "units": self.units,
            "value_type": self.value_ty, "min": self.min_val,
            "max": self.max_val}}

class API:
  def __init__(self, plugin_id, frequency = "Five_Seconds"):
    self.uid = plugin_id
    self.datasources = []
    self.frequency = frequency
    self.frequency_in_seconds = self.frequency_to_seconds(frequency)
    p = Proxy("http://_var_xapi_xcp-rrdd/", transport = UDSTransport())
    self.dispatcher = Dispatcher(p.request, None).Plugin

  def lazy_complete_init(self):
    """This part of API initialisation can fail, since it relies on the
    xcp-rrdd daemon to be up. Therefore, instead of calling it immediately,
    it is called lazily within wait_until_next_reading."""
    if hasattr(self, 'dest'): return
    self.header = self.dispatcher.get_header()
    self.path = self.dispatcher.get_path({"uid": self.uid})
    base_path = os.path.dirname(self.path)
    if not os.path.exists(base_path): os.makedirs(base_path)
    self.dest = open(self.path, "w")

  def __del__(self):
    self.deregister()

  def frequency_to_seconds(self, frequency):
    if frequency == "Five_Seconds": return 5
    raise NameError(frequency)

  def set_datasource(self, name, value, description = "", units = "",
               ty = DataSource.Type.ABSOLUTE,
               value_ty = DataSource.ValueType.FLOAT,
               min_val = "-infinity", max_val = "infinity"):
    """This function should be called within each iteration of the plugin,
    and for each datasource, every time providing freshly collected data."""
    ds = DataSource(name, value, description, units, ty, value_ty,
                    min_val, max_val)
    self.datasources.append(ds)

  def get_header(self):
    """Get the 'static' first line of the expected output format."""
    return self.header

  def get_path(self):
    """Get the path of the file in which to write the results to."""
    return self.path

  def register(self):
    """Register plugin if not already registered, and return next_reading."""
    params = {"uid": self.uid, "frequency": self.frequency}
    return self.dispatcher.register(params)

  def deregister(self):
    """De-register a plugin. Called in destructor. """
    return self.dispatcher.deregister({"uid": self.uid})

  def next_reading(self):
    """Return the time period after which the next reading for this plugin
    will take place."""
    return self.dispatcher.next_reading({"uid": self.uid})

  def wait_until_next_reading(self, neg_shift = 1):
    """The xcp-rrdd daemon reads the files written by registered plugins in
    pre-determined time intervals. This function coordinates this timing with
    the daemon, and wakes up just before the next such reading occurs. This
    way, the plugin can provide freshly collected data. The neg_shift argument
    specifies (in seconds; can be a fraction; default is 1) how long before
    the next reading this function return. Note that it is up to the plugin
    author to choose a value for neg_shift that is at least as large the time
    it takes for the plugin to collect its data; however, it should also not
    be much larger, since this decreases the freshness of the data."""
    while True:
      try:
        self.lazy_complete_init()
        next_reading = self.register()
        wait_time = next_reading - neg_shift
        if wait_time < 0: wait_time %= self.frequency_in_seconds
        time.sleep(wait_time)
        return
      except socket.error:
        msg = "Failed to contact xcp-rrdd. Sleeping for 5 seconds .."
        print >> sys.stderr, msg
        time.sleep(5)

  def update(self):
    """Write all datasources specified (via set_datasource) since the last
    call to this function. The datasources are written together with the
    relevant metadata into the file agreed with rrdd."""
    timestamp = int(time.time())
    combined = dict()
    for ds in self.datasources:
      combined = dict(combined.items() + ds.to_dict().items())
    payload = {"timestamp": timestamp, "datasources": combined}
    payload_json = json.dumps(payload, sort_keys = True, indent = 2)
    payload_pretty = '\n'.join([l.rstrip() for l in payload_json.splitlines()])
    m = md5.new()
    m.update(payload_pretty)
    output = ""
    output += self.header
    output += "%08X\n" % len(payload_pretty)
    output += m.hexdigest() + "\n"
    output += payload_pretty
    self.dest.seek(0)
    self.dest.write(output)
    self.dest.flush()
    self.datasources = []
    time.sleep(0.003) # wait a bit to ensure wait_until_next_reading will block
