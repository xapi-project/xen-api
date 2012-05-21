#!/usr/bin/env python

import storage
import xmlrpclib
import unittest

base_path = "../rpc-light/"

def read_file(filename):
    f = open(filename, "r")
    try:
        return f.read()
    finally:
        f.close()

def response_from(filename):
    """Return a pre-generated response from [filename]"""
    xml = read_file(base_path + filename)
    (result, ), name = xmlrpclib.loads(xml)
    if result["Status"] == "Success":
        return result["Value"]
    else:
        raise "I don't know how to deal with failures"

def request_from(filename):
    """Return a pre-generated request from [filename]"""
    xml = read_file(base_path + filename)
    (args, ), name = xmlrpclib.loads(xml)
    return args

class SR_dummy:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def attach(self, sr, device_config):
        return response_from("sr.attach/response")
    def detach(self, sr):
        return response_from("sr.detach/response")
    def destroy(self, sr):
        return response_from("sr.destroy/response")
    def reset(self, sr):
        return response_from("sr.reset/response")
    def scan(self, sr):
        return response_from("sr.scan/response")

class Test_SR(unittest.TestCase):
    def setUp(self):
        self.x = storage.SR_server_dispatcher(SR_dummy())

    def test_attach(self):
        self.x.attach(request_from("sr.attach/request"))
    def test_detach(self):
        self.x.detach(request_from("sr.detach/request"))
    def test_scan(self):
        self.x.scan(request_from("sr.scan/request"))

class VDI_test:
    """Operations which operate on Virtual Disk Images"""
    def __init__(self):
        pass
    def create(self, sr, vdi_info, params):
        return response_from("vdi.create/response")
    def snapshot(self, sr, vdi, vdi_info, params):
        return response_from("vdi.snapshot/response")
    def clone(self, sr, vdi, vdi_info, params):
        return response_from("vdi.clone/response")
    def destroy(self, sr, vdi):
        return response_from("vdi.destroy/response")
    def attach(self, dp, sr, vdi, read_write):
        return response_from("vdi.attach/response")
    def activate(self, dp, sr, vdi):
        return response_from("vdi.activate/response")
    def deactivate(self, dp, sr, vdi):
        return response_from("vdi.deactivate/response")
    def detach(self, dp, sr, vdi):
        return response_from("vdi.detach/response")

class Test_VDI(unittest.TestCase):
    def setUp(self):
        self.x = storage.VDI_server_dispatcher(VDI_test())

    def test_create(self):
        self.x.create(request_from("vdi.create/request"))
    def test_snapshot(self):
        self.x.snapshot(request_from("vdi.snapshot/request"))
    def test_clone(self):
        self.x.clone(request_from("vdi.clone/request"))
    def test_destroy(self):
        self.x.destroy(request_from("vdi.destroy/request"))
    def test_attach(self):
        self.x.attach(request_from("vdi.attach/request"))
    def test_activate(self):
        self.x.activate(request_from("vdi.activate/request"))
    def test_deactivate(self):
        self.x.deactivate(request_from("vdi.deactivate/request"))
    def test_detach(self):
        self.x.detach(request_from("vdi.detach/request"))

if __name__ == '__main__':
    unittest.main()
