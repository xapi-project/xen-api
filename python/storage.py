from xcp import *
import traceback
class Sr_not_attached(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Sr_not_attached", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Volume_does_not_exist(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Volume_does_not_exist", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Backend_error(Rpc_light_failure):
    def __init__(self, arg_0, arg_1):
        Rpc_light_failure.__init__(self, "Backend_error", [ arg_0, arg_1 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        if type(arg_1) <> type([]):
            raise (TypeError("string list", repr(arg_1)))
        for x in arg_1:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        self.arg_0 = arg_0
        self.arg_1 = arg_1
class Unimplemented(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Unimplemented", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Does_not_exist(Rpc_light_failure):
    def __init__(self, arg_0, arg_1):
        Rpc_light_failure.__init__(self, "Does_not_exist", [ arg_0, arg_1 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        if type(arg_1) <> type(""):
            raise (TypeError("string", repr(arg_1)))
        self.arg_0 = arg_0
        self.arg_1 = arg_1
class Cancelled(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Cancelled", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Sr_attached(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Sr_attached", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Driver_server_dispatcher:
    """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def query(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.query(dbg)
        if type(results['driver']) <> type(""):
            raise (TypeError("string", repr(results['driver'])))
        if type(results['name']) <> type(""):
            raise (TypeError("string", repr(results['name'])))
        if type(results['description']) <> type(""):
            raise (TypeError("string", repr(results['description'])))
        if type(results['vendor']) <> type(""):
            raise (TypeError("string", repr(results['vendor'])))
        if type(results['copyright']) <> type(""):
            raise (TypeError("string", repr(results['copyright'])))
        if type(results['version']) <> type(""):
            raise (TypeError("string", repr(results['version'])))
        if type(results['required_api_version']) <> type(""):
            raise (TypeError("string", repr(results['required_api_version'])))
        if type(results['features']) <> type([]):
            raise (TypeError("string list", repr(results['features'])))
        for x in results['features']:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        if type(results['configuration']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['configuration'])))
        for x in results['configuration'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['configuration'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def ls(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.ls(dbg)
        if type(results) <> type([]):
            raise (TypeError("string list", repr(results)))
        for x in results:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def diagnostics(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.diagnostics(dbg)
        if type(results) <> type(""):
            raise (TypeError("string", repr(results)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "Driver.query":
            return success(self.query(args))
        elif method == "Driver.ls":
            return success(self.ls(args))
        elif method == "Driver.diagnostics":
            return success(self.diagnostics(args))
class Driver_skeleton:
    """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        raise Unimplemented("Driver.query")
    def ls(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        raise Unimplemented("Driver.ls")
    def diagnostics(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        raise Unimplemented("Driver.diagnostics")
class Driver_test:
    """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        result = {}
        result["query_result"] = { "driver": "string", "name": "string", "description": "string", "vendor": "string", "copyright": "string", "version": "string", "required_api_version": "string", "features": [ "string", "string" ], "configuration": { "string": "string" } }
        return result
    def ls(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        result = {}
        result["srs"] = [ "string", "string" ]
        return result
    def diagnostics(self, dbg):
        """Discover properties of this implementation. Every implementation must support the query interface or it will not be recognised as a storage manager by xapi."""
        result = {}
        result["diagnostics"] = "string"
        return result
class Volume_server_dispatcher:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def create(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('name_label')):
            raise UnmarshalException('argument missing', 'name_label', '')
        name_label = args["name_label"]
        if type(name_label) <> type(""):
            raise (TypeError("string", repr(name_label)))
        if not(args.has_key('name_description')):
            raise UnmarshalException('argument missing', 'name_description', '')
        name_description = args["name_description"]
        if type(name_description) <> type(""):
            raise (TypeError("string", repr(name_description)))
        if not(args.has_key('size')):
            raise UnmarshalException('argument missing', 'size', '')
        size = args["size"]
        if not(is_long(size)):
            raise (TypeError("int64", repr(size)))
        results = self._impl.create(dbg, sr, name_label, name_description, size)
        if type(results['key']) <> type(""):
            raise (TypeError("string", repr(results['key'])))
        if type(results['name']) <> type(""):
            raise (TypeError("string", repr(results['name'])))
        if type(results['description']) <> type(""):
            raise (TypeError("string", repr(results['description'])))
        if type(results['read_write']) <> type(True):
            raise (TypeError("bool", repr(results['read_write'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if type(results['uri']) <> type([]):
            raise (TypeError("string list", repr(results['uri'])))
        for x in results['uri']:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def snapshot(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        results = self._impl.snapshot(dbg, sr)
        if type(results['key']) <> type(""):
            raise (TypeError("string", repr(results['key'])))
        if type(results['name']) <> type(""):
            raise (TypeError("string", repr(results['name'])))
        if type(results['description']) <> type(""):
            raise (TypeError("string", repr(results['description'])))
        if type(results['read_write']) <> type(True):
            raise (TypeError("bool", repr(results['read_write'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if type(results['uri']) <> type([]):
            raise (TypeError("string list", repr(results['uri'])))
        for x in results['uri']:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def clone(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        results = self._impl.clone(dbg, sr)
        if type(results['key']) <> type(""):
            raise (TypeError("string", repr(results['key'])))
        if type(results['name']) <> type(""):
            raise (TypeError("string", repr(results['name'])))
        if type(results['description']) <> type(""):
            raise (TypeError("string", repr(results['description'])))
        if type(results['read_write']) <> type(True):
            raise (TypeError("bool", repr(results['read_write'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if type(results['uri']) <> type([]):
            raise (TypeError("string list", repr(results['uri'])))
        for x in results['uri']:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def destroy(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('key')):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if type(key) <> type(""):
            raise (TypeError("string", repr(key)))
        results = self._impl.destroy(dbg, sr, key)
        return results
    def resize(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('key')):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if type(key) <> type(""):
            raise (TypeError("string", repr(key)))
        if not(args.has_key('new_size')):
            raise UnmarshalException('argument missing', 'new_size', '')
        new_size = args["new_size"]
        if not(is_long(new_size)):
            raise (TypeError("int64", repr(new_size)))
        results = self._impl.resize(dbg, sr, key, new_size)
        return results
    def stat(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('key')):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if type(key) <> type(""):
            raise (TypeError("string", repr(key)))
        results = self._impl.stat(dbg, sr, key)
        if type(results['key']) <> type(""):
            raise (TypeError("string", repr(results['key'])))
        if type(results['name']) <> type(""):
            raise (TypeError("string", repr(results['name'])))
        if type(results['description']) <> type(""):
            raise (TypeError("string", repr(results['description'])))
        if type(results['read_write']) <> type(True):
            raise (TypeError("bool", repr(results['read_write'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if type(results['uri']) <> type([]):
            raise (TypeError("string list", repr(results['uri'])))
        for x in results['uri']:
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def copy(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('key')):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if type(key) <> type(""):
            raise (TypeError("string", repr(key)))
        if not(args.has_key('url')):
            raise UnmarshalException('argument missing', 'url', '')
        url = args["url"]
        if type(url) <> type(""):
            raise (TypeError("string", repr(url)))
        if not(args.has_key('dest')):
            raise UnmarshalException('argument missing', 'dest', '')
        dest = args["dest"]
        if type(dest) <> type(""):
            raise (TypeError("string", repr(dest)))
        results = self._impl.copy(dbg, sr, key, url, dest)
        if type(results) <> type(""):
            raise (TypeError("string", repr(results)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "Volume.create":
            return success(self.create(args))
        elif method == "Volume.snapshot":
            return success(self.snapshot(args))
        elif method == "Volume.clone":
            return success(self.clone(args))
        elif method == "Volume.destroy":
            return success(self.destroy(args))
        elif method == "Volume.resize":
            return success(self.resize(args))
        elif method == "Volume.stat":
            return success(self.stat(args))
        elif method == "Volume.copy":
            return success(self.copy(args))
class Volume_skeleton:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self):
        pass
    def create(self, dbg, sr, name_label, name_description, size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.create")
    def snapshot(self, dbg, sr):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.snapshot")
    def clone(self, dbg, sr):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.clone")
    def destroy(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.destroy")
    def resize(self, dbg, sr, key, new_size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.resize")
    def stat(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.stat")
    def copy(self, dbg, sr, key, url, dest):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.copy")
class Volume_test:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self):
        pass
    def create(self, dbg, sr, name_label, name_description, size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["new_volume"] = { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] }
        return result
    def snapshot(self, dbg, sr):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["new_volume"] = { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] }
        return result
    def clone(self, dbg, sr):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["new_volume"] = { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] }
        return result
    def destroy(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def resize(self, dbg, sr, key, new_size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def stat(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["volume"] = { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] }
        return result
    def copy(self, dbg, sr, key, url, dest):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["new_volume"] = "string"
        return result
class SR_server_dispatcher:
    """Operations which act on Storage Repositories"""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def create(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('physical_size')):
            raise UnmarshalException('argument missing', 'physical_size', '')
        physical_size = args["physical_size"]
        if not(is_long(physical_size)):
            raise (TypeError("int64", repr(physical_size)))
        results = self._impl.create(dbg, sr, physical_size)
        return results
    def attach(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('device_config')):
            raise UnmarshalException('argument missing', 'device_config', '')
        device_config = args["device_config"]
        if type(device_config) <> type({}):
            raise (TypeError("(string * string) list", repr(device_config)))
        for x in device_config.keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in device_config.values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        results = self._impl.attach(dbg, sr, device_config)
        return results
    def detach(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        results = self._impl.detach(dbg, sr)
        return results
    def destroy(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        results = self._impl.destroy(dbg, sr)
        return results
    def scan(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        results = self._impl.scan(dbg, sr)
        if type(results) <> type([]):
            raise (TypeError("87 list", repr(results)))
        for x in results:
            if type(x['key']) <> type(""):
                raise (TypeError("string", repr(x['key'])))
            if type(x['name']) <> type(""):
                raise (TypeError("string", repr(x['name'])))
            if type(x['description']) <> type(""):
                raise (TypeError("string", repr(x['description'])))
            if type(x['read_write']) <> type(True):
                raise (TypeError("bool", repr(x['read_write'])))
            if not(is_long(x['virtual_size'])):
                raise (TypeError("int64", repr(x['virtual_size'])))
            if type(x['uri']) <> type([]):
                raise (TypeError("string list", repr(x['uri'])))
            for x in x['uri']:
                if type(x) <> type(""):
                    raise (TypeError("string", repr(x)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "SR.create":
            return success(self.create(args))
        elif method == "SR.attach":
            return success(self.attach(args))
        elif method == "SR.detach":
            return success(self.detach(args))
        elif method == "SR.destroy":
            return success(self.destroy(args))
        elif method == "SR.scan":
            return success(self.scan(args))
class SR_skeleton:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def create(self, dbg, sr, physical_size):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.create")
    def attach(self, dbg, sr, device_config):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.attach")
    def detach(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.detach")
    def destroy(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.destroy")
    def scan(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.scan")
class SR_test:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def create(self, dbg, sr, physical_size):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def attach(self, dbg, sr, device_config):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def detach(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def destroy(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def scan(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        result["volumes"] = [ { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] }, { "key": "string", "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "uri": [ "string", "string" ] } ]
        return result
class storage_server_dispatcher:
    """Demux calls to individual interface server_dispatchers"""
    def __init__(self, Driver = None, Volume = None, SR = None):
        self.Driver = Driver
        self.Volume = Volume
        self.SR = SR
    def _dispatch(self, method, params):
        try:
            log("method = %s params = %s" % (method, repr(params)))
            if method.startswith("Driver") and self.Driver:
                return self.Driver._dispatch(method, params)
            elif method.startswith("Volume") and self.Volume:
                return self.Volume._dispatch(method, params)
            elif method.startswith("SR") and self.SR:
                return self.SR._dispatch(method, params)
            raise UnknownMethod(method)
        except Exception, e:
            log("caught %s" % e)
            traceback.print_exc()
            try:
                # A declared (expected) failure will have a .failure() method
                log("returning %s" % (repr(e.failure())))
                return e.failure()
            except:
                # An undeclared (unexpected) failure is wrapped as InternalError
                return (InternalError(str(e)).failure())
class storage_server_test(storage_server_dispatcher):
    """Create a server which will respond to all calls, returning arbitrary values. This is intended as a marshal/unmarshal test."""
    def __init__(self):
        storage_server_dispatcher.__init__(self, Driver_server_dispatcher(Driver_test()), Volume_server_dispatcher(Volume_test()), SR_server_dispatcher(SR_test()))