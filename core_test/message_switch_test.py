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

from message_switch import *

basedir = "/tmp/link_test"

rpc_req = Message("hello", 1L, "reply_to")
rpc_res = Message("hello", 1L)

import unittest, os
class Internal_invariants(unittest.TestCase):
    def test_Message_save_load(self):
        for m in [rpc_req, rpc_res]:
            n = Message.load(m.save())
            assert m.payload == n.payload
            assert m.correlation_id == n.correlation_id
            assert m.reply_to == n.reply_to

def load(x):
    path = os.path.join(basedir, x)
    f = open(path, "r")
    try:
        return f.read()
    finally:
        f.close()

class Ocaml_interop(unittest.TestCase):
    def test_login(self):
        py = Login("hello").to_request().to_string()
        ocaml = load("login")
        assert py == ocaml
    def test_create_named(self):
        py = Create_request("service").to_request().to_string()
        ocaml = load("create")
        assert py == ocaml
    def test_create_anon(self):
        py = Create_request().to_request().to_string()
        ocaml = load("create.anon")
        assert py == ocaml
    def test_subscribe(self):
        py = Subscribe("service").to_request().to_string()
        ocaml = load("subscribe")
        assert py == ocaml
    def test_request(self):
        py = Send("service", rpc_req).to_request().to_string()
        ocaml = load("request")
        assert py == ocaml
    def test_response(self):
        py = Send("service", rpc_res).to_request().to_string()
        ocaml = load("reply")
        assert py == ocaml
    def test_transfer(self):
        py = Transfer_request(3, 5.0).to_request().to_string()
        ocaml = load("transfer")
        assert py == ocaml
    def test_ack(self):
        py = Ack(3).to_request().to_string()
        ocaml = load("ack")
        assert py == ocaml

    def test_create_reply(self):
        ocaml = Create_response.of_response(Http_response.of_string(load("create.reply")))
        assert ocaml.name == "service"
    def test_transfer_reply(self):
        ocaml = Transfer_response.of_response(Http_response.of_string(load("transfer.reply")))
        m = {
            1L: rpc_req,
            2L: rpc_res,
            }
        py = Transfer_response(m)
        for k in py.messages:
            assert k in ocaml.messages
            assert str(py.messages[k]) == str(ocaml.messages[k])
        for k in ocaml.messages:
            assert k in py.messages
            assert str(py.messages[k]) == str(ocaml.messages[k])

if __name__ == "__main__":
	unittest.main()
