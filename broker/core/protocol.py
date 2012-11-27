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
            "Content-length: %d" % len(body),
            "",
            body
            ]
        return "\r\n".join(lines)

class Http_response:
    def __init__(self, body):
        self.body = body

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

class Subscribe:
    def __init__(self, name):
        self.name = name

    def to_request(self):
        return Http_request("GET", "/subscribe/%s" % name)

class Send:
    def __init__(self, name, message, correlation_id, reply_to = None):
        self.name = name
        self.message = message
        self.correlation_id = correlation_id
        self.reply_to = reply_to
    def to_request(self):
        if self.reply_to:
            return Http_request("POST", "/send/%s/%d/%s" % (self.name, self.correlation_id, self.reply_to), self.message)
        else:
            return Http_request("POST", "/send/%s/%d" % (self.name, self.correlation_id), self.message)

class Transfer_request:
    def __init__(self, ack_to, timeout):
        self.id = ack_to
        self.timeout = timeout

    def to_request(self):
        return Http_request("GET", "/transfer/%Ld/%.16g" % (self.ack_to, self.timeout))

class Transfer_response:
    def __init__(self, messages):
        self.messages

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
