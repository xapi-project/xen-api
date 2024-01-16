#!/usr/bin/env python

# Copyright (c) 2012 Citrix Systems Inc
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

import json

class Http_request:
    def __init__(self, method, uri, body = None):
        self.method = method
        self.uri = uri
        self.body = body

    def to_string(self):
        body = ""
        if self.body:
            body = self.body
        lines = [
            "%s %s HTTP/1.1" % (self.method, self.uri),
            "Content-Length: %d" % len(body),
            "",
            body
            ]
        return "\r\n".join(lines)

class Http_response:
    def __init__(self, body):
        self.body = body

    def to_string(self):
        lines = [
            "HTTP/1.1 200 OK",
            "Content-Length: %d" % len(self.body),
            "",
            self.body
            ]
        return "\r\n".join(lines)

    @classmethod
    def of_string(cls, txt):
        lines = txt.split("\r\n")
        if lines[0] <> "HTTP/1.1 200 OK":
            raise "Unexpected status line: %s" % lines[0]
        rest = "\r\n".join(lines[3:])
        return cls(rest)

class Message:
    def __init__(self, payload, correlation_id, reply_to = None):
        self.payload = payload
        self.correlation_id = correlation_id
        self.reply_to = reply_to

    def save(self):
        result = {
            "payload": self.payload,
            "correlation_id": self.correlation_id
            }
        if self.reply_to:
            result["reply_to"] = self.reply_to
        return result

    @classmethod
    def load(cls, x):
        payload = x["payload"]
        correlation_id = x["correlation_id"]
        reply_to = None
        if "reply_to" in x:
            reply_to = x["reply_to"]
        return cls(payload, correlation_id, reply_to)

    def __str__(self):
        return json.dumps(self.save())

class Login:
    def __init__(self, some_credential):
        self.some_credential = some_credential

    def to_request(self):
        return Http_request("GET", "/login/%s" % self.some_credential)

class Create_request:
    def __init__(self, name = None):
        self.name = name

    def to_request(self):
        uri = "/create"
        if self.name:
            uri = uri + "/" + self.name
        return Http_request("GET", uri)

class Create_response:
    def __init__(self, name = None):
        self.name = name

    @classmethod
    def of_response(cls, response):
        return cls(response.body)

    def to_response(self):
        return Http_response(self.name)

class Subscribe:
    def __init__(self, name):
        self.name = name

    def to_request(self):
        return Http_request("GET", "/subscribe/%s" % self.name)

class Send:
    def __init__(self, name, message):
        self.name = name
        self.message = message
    def to_request(self):
        if self.message.reply_to:
            return Http_request("POST", "/send/%s/%d/%s" % (self.name, self.message.correlation_id, self.message.reply_to), self.message.payload)
        else:
            return Http_request("POST", "/send/%s/%d" % (self.name, self.message.correlation_id), self.message.payload)

class Transfer_request:
    def __init__(self, ack_to, timeout):
        self.ack_to = ack_to
        self.timeout = timeout

    def to_request(self):
        return Http_request("GET", "/transfer/%Ld/%.16g" % (self.ack_to, self.timeout))

class Transfer_response:
    def __init__(self, messages):
        self.messages = messages

    @classmethod
    def of_response(cls, response):
        x = json.loads(response.body)
        result = {}
        for (k, v) in x["messages"]:
            result[long(k)] = Message.load(v)
        return Transfer_response(result)

class Ack:
    def __init__(self, ack):
        self.ack = ack

    def to_request(self):
        return Http_request("GET", "/ack/%Ld" % self.ack)

import string, socket

default_config = {
    "ip": "169.254.0.1", # HIMN IP of dom0
    "port": 8080,      # default for xenswitch
}

class End_of_file(Exception):
    def __init__(self):
        pass
class Bad_status(Exception):
    def __init__(self, status):
        self.status = status
class Missing_content_length(Exception):
    def __init__(self):
        pass
class StreamReader:
    def __init__(self, sock):
        self.sock = sock
        self.buffered = ""
    def read_fragment(self, n):
        if len(self.buffered) > 0:
            num_available = min(n, len(self.buffered))
            fragment = self.buffered[0:num_available]
            self.buffered = self.buffered[num_available:]
            return fragment
        else:
            self.buffered = self.sock.recv(16384)
            if len(self.buffered) == 0:
                raise End_of_file()
            return self.read_fragment(n)
    def read(self, n):
        results = ""
        while n > 0:
            fragment = self.read_fragment(n)
            n = n - len(fragment)
            results = results + fragment
        return results

    def readline(self):
        results = ""
        eol = False
        while not eol:
            byte = self.read(1)
            if byte == "\n":
                eol = True
            else:
                results = results + byte
        return results

def link_send(sock, m):
    sock.sendall(m.to_request().to_string())

def link_recv(reader):
    status = reader.readline()
    if not(status.startswith("HTTP/1.1 200 OK")):
        raise Bad_status(status)
    content_length = None
    eoh = False
    while not eoh:
        header = reader.readline().strip()
        if header == "":
            eoh = True
        else:
            bits = header.split(":")
            key = string.lower(bits[0])
            if key == "content-length":
                content_length = int(bits[1])
    if content_length == None:
        raise Missing_content_length()
    body = reader.read(content_length)
    return Http_response(body)

def login(sock, reader, some_credential):
    link_send(sock, Login(some_credential))
    link_recv(reader)

def create(sock, reader, name = None):
    link_send(sock, Create_request(name))
    return Create_response.of_response(link_recv(reader)).name

def subscribe(sock, reader, name):
    link_send(sock, Subscribe(name))
    link_recv(reader)

def send(sock, reader, name, msg):
    link_send(sock, Send(name, msg))
    link_recv(reader)

def transfer(sock, reader, ack_to, timeout):
    link_send(sock, Transfer_request(ack_to, timeout))
    return Transfer_response.of_response(link_recv(reader)).messages

def ack(sock, reader, id):
    link_send(sock, Ack(id))
    link_recv(reader)

from threading import Thread, Event, Lock

class Receiver(Thread):
    def __init__(self, sock, reader, server):
        Thread.__init__(self)
        self.daemon = True
        self.sock = sock
        self.reader = reader
        self.server = server
        self.events = {}
        self.replies = {}
    def register_correlation_id(self, correlation_id):
        event = Event()
        self.events[correlation_id] = event
        return event
    def get_reply(self, correlation_id):
        reply = self.replies[correlation_id]
        del self.replies[correlation_id]
        return reply
    def set_listen_callback(self, listen_callback):
        self.listen_callback = listen_callback
    def run(self):
        ack_to = -1
        timeout = 5.0
        while True:
            messages = transfer(self.sock, self.reader, ack_to, timeout)
            for id in messages.keys():
                ack_to = max(ack_to, id)
                m = messages[id]
                reply_to = m.reply_to
                if reply_to:
                    reply = self.server.dispatch(m)
                    send(self.sock, self.reader, reply_to, reply)
                    ack(self.sock, self.reader, id)
                else:
                    if m.correlation_id not in self.events:
                        print >>sys.stderr, "Unknown correlation_id: %d" % m.correlation_id
                    else:
                        self.replies[m.correlation_id] = m.payload
                        event = self.events[m.correlation_id]
                        del self.events[m.correlation_id]
                        event.set()

class Connection:
    def __init__(self, client, name):
        self.client = client
        self.name = name
    def rpc(self, request):
        return self.client.rpc(self.name, request)

class Server:
    def __init__(self):
        pass
    def dispatch(self, request):
        # echo the request back
        request.reply_to = None
        return request

class Switch:
    def __init__(self, some_credential, config = default_config, server = Server()):
        self.some_credential = some_credential
        self.config = config
        self.server = server

        # Open a connection for requests and one for events
        self.request_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.request_sock.connect((config["ip"], config["port"]))
        self.request_stream_reader = StreamReader(self.request_sock)
        self.request_mutex = Lock()
        login(self.request_sock, self.request_stream_reader, some_credential)

        self.event_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.event_sock.connect((config["ip"], config["port"]))
        self.event_stream_reader = StreamReader(self.event_sock)
        login(self.event_sock, self.event_stream_reader, some_credential)

        self.receiver_thread = Receiver(self.event_sock, self.event_stream_reader, self.server)
        self.receiver_thread.start()
        self.next_correlation_id = 0
        self.next_correlation_id_mutex = Lock()

    def correlation_id(self):
        self.next_correlation_id_mutex.acquire()
        try:
            correlation_id = self.next_correlation_id
            self.next_correlation_id = self.next_correlation_id + 1
            return correlation_id
        finally:
            self.next_correlation_id_mutex.release()

    def rpc(self, name, request):
        correlation_id = self.correlation_id()
        event = self.receiver_thread.register_correlation_id(correlation_id)

        self.request_mutex.acquire()
        try:
            reply_queue = create(self.request_sock, self.request_stream_reader)
            subscribe(self.request_sock, self.request_stream_reader, reply_queue)
            send(self.request_sock, self.request_stream_reader, name, Message(request, correlation_id, reply_queue))
        finally:
            self.request_mutex.release()

        event.wait()
        return self.receiver_thread.get_reply(correlation_id)

    def connect(self, service):
        self.request_mutex.acquire()
        try:
            create(self.request_sock, self.request_stream_reader, service)
        finally:
            self.request_mutex.release()

        return Connection(self, service)

    def listen(self, service):
        self.request_mutex.acquire()
        try:
            create(self.request_sock, self.request_stream_reader, service)
            subscribe(self.request_sock, self.request_stream_reader, service)
        finally:
            self.request_mutex.release()


if __name__ == "__main__":
    from optparse import OptionParser
    import sys, time

    parser = OptionParser()
    parser.add_option("-x", "--switch", dest="switch", type="string",
                  help="address of message switch", metavar="SWITCH")
    parser.add_option("-l", "--listen", dest="listen", action="store_true",
                  help="listen for RPCs, instead of sending them")
    parser.add_option("-s", "--service", dest="service", type="string",
                  help="name of the remote service")
    parser.add_option("-c", "--client", dest="client_name", type="string",
                  help="name which identifies this client")

    (options, args) = parser.parse_args()
    config = default_config
    if options.switch:
        bits = options.switch.split(":")
        config["ip"] = bits[0]
        if len(bits) == 2:
            config["port"] = int(bits[1])

    client_name = "test_python"
    if options.client_name:
        client_name = options.client_name
    if not options.service:
        print >> sys.stderr, "Must provide a --service name"
        sys.exit(1)

    if options.listen:
        s = Switch(client_name, server = Server())
        s.listen(options.service)
        while True:
            time.sleep(5)
    else:
        s = Switch(client_name)
        c = s.connect(options.service)
        print c.rpc("hello")
