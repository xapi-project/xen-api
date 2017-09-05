#!/usr/bin/python
"""Test functions for guestcommandlib.py"""

import guestcommandlib

class DumbSerial(object):
    """Pretend to be a serial device"""
    count = 0
    def __init__(self, input_string):
        self.input = input_string
    def read(self, size=1):
        """Read a byte from the 'device'"""
        if self.count >= len(self.input):
            raise Exception('Called too many')
        byte_read = self.input[self.count]
        self.count = self.count +1
        return byte_read

def collate(stream, count):
    """Fetch up to count bytes froma stream"""
    out = b""
    for i in stream:
        out = b"".join([out, i])
        count = count - 1
        if count == 0:
            return out
    return out

def test_simple_unicod_stream():
    """Test we can receive a string from a UnicodeStream"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(b"Hello World")).await_ctrl_code('d'), 4)
    print(res)
    assert res == b"Hell"

def test_wait_for_ctrl_code():
    """Test that UnicodeStream can terminate on a control code"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(b"Hello World")).await_ctrl_code('o'), 6)
    print(res)
    assert res == b"Hell"

def test_ctrl_code_at_start():
    """Test we can cope with a string starting with a control code"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(b"Hello World")).await_ctrl_code('H'), 11)
    print(res)
    assert res == b""

def test_no_ctrl_code():
    """Test we can cope if the string ends with no control code being found"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(b"Hello World")).await_ctrl_code('z'), 11)
    print(res)
    assert res == b"Hello World"

DATA = b"Test\xe3\x82\xa0World"

def test_with_unicode_characters():
    """Check we pass whole unicode chartacters through"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(DATA)).await_ctrl_code('W'), 6)
    print(res)
    assert res == b"Test\xe3\x82\xa0"

def test_with_unicode_start_ctrl():
    """Ensure that start bytes of unicode bytes don't count as control codes"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(DATA)).await_ctrl_code('\xe3'), 9)
    print(res)
    assert res == b"Test\xe3\x82\xa0Worl"

def test_unicode_continuation_ctrl():
    """Test that continuation unicode bytes don't count as control codes"""
    res = collate(
        guestcommandlib.UnicodeStream(DumbSerial(DATA)).await_ctrl_code('\xa0'), 9)
    print(res)
    assert res == b"Test\xe3\x82\xa0Worl"
