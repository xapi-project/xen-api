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

from protocol import *

rpc_req = Message("hello", 1L, "reply_here")
rpc_res = Message("there", 1L)

import unittest, os
class Internal_invariants(unittest.TestCase):
    def test_Message_save_load(self):
        for m in [rpc_req, rpc_res]:
            n = Message.load(m.save())
            assert m.payload == n.payload
            assert m.correlation_id == n.correlation_id
            assert m.reply_to == n.reply_to


if __name__ == "__main__":
	unittest.main()
