from xapi import success, Rpc_light_failure, InternalError, UnmarshalException, TypeError, is_long, UnknownMethod
import xapi
import sys
import json
import argparse
import traceback
import logging
class Sr_not_attached(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Sr_not_attached", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise TypeError("string", repr(arg_0))
        self.arg_0 = arg_0
class SR_does_not_exist(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "SR_does_not_exist", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise TypeError("string", repr(arg_0))
        self.arg_0 = arg_0
class Volume_does_not_exist(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Volume_does_not_exist", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise TypeError("string", repr(arg_0))
        self.arg_0 = arg_0
class Unimplemented(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Unimplemented", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise TypeError("string", repr(arg_0))
        self.arg_0 = arg_0
class Cancelled(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Cancelled", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise TypeError("string", repr(arg_0))
        self.arg_0 = arg_0
class Volume_server_dispatcher:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def create(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('name' in args):
            raise UnmarshalException('argument missing', 'name', '')
        name = args["name"]
        if not isinstance(name, str) and not isinstance(name, unicode):
            raise TypeError("string", repr(name))
        if not('description' in args):
            raise UnmarshalException('argument missing', 'description', '')
        description = args["description"]
        if not isinstance(description, str) and not isinstance(description, unicode):
            raise TypeError("string", repr(description))
        if not('size' in args):
            raise UnmarshalException('argument missing', 'size', '')
        size = args["size"]
        if not(is_long(size)):
            raise TypeError("int64", repr(size))
        results = self._impl.create(dbg, sr, name, description, size)
        if not isinstance(results['key'], str) and not isinstance(results['key'], unicode):
            raise TypeError("string", repr(results['key']))
        if results['uuid'] is not None:
            if not isinstance(results['uuid'], str) and not isinstance(results['uuid'], unicode):
                raise TypeError("string", repr(results['uuid']))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise TypeError("string", repr(results['name']))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise TypeError("string", repr(results['description']))
        if not isinstance(results['read_write'], bool):
            raise TypeError("bool", repr(results['read_write']))
        if not(is_long(results['virtual_size'])):
            raise TypeError("int64", repr(results['virtual_size']))
        if not(is_long(results['physical_utilisation'])):
            raise TypeError("int64", repr(results['physical_utilisation']))
        if not isinstance(results['uri'], list):
            raise TypeError("string list", repr(results['uri']))
        for tmp_5 in results['uri']:
            if not isinstance(tmp_5, str) and not isinstance(tmp_5, unicode):
                raise TypeError("string", repr(tmp_5))
        if not isinstance(results['keys'], dict):
            raise TypeError("(string * string) list", repr(results['keys']))
        for tmp_6 in results['keys'].keys():
            if not isinstance(tmp_6, str) and not isinstance(tmp_6, unicode):
                raise TypeError("string", repr(tmp_6))
        for tmp_6 in results['keys'].values():
            if not isinstance(tmp_6, str) and not isinstance(tmp_6, unicode):
                raise TypeError("string", repr(tmp_6))
        return results
    def snapshot(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        results = self._impl.snapshot(dbg, sr, key)
        if not isinstance(results['key'], str) and not isinstance(results['key'], unicode):
            raise TypeError("string", repr(results['key']))
        if results['uuid'] is not None:
            if not isinstance(results['uuid'], str) and not isinstance(results['uuid'], unicode):
                raise TypeError("string", repr(results['uuid']))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise TypeError("string", repr(results['name']))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise TypeError("string", repr(results['description']))
        if not isinstance(results['read_write'], bool):
            raise TypeError("bool", repr(results['read_write']))
        if not(is_long(results['virtual_size'])):
            raise TypeError("int64", repr(results['virtual_size']))
        if not(is_long(results['physical_utilisation'])):
            raise TypeError("int64", repr(results['physical_utilisation']))
        if not isinstance(results['uri'], list):
            raise TypeError("string list", repr(results['uri']))
        for tmp_7 in results['uri']:
            if not isinstance(tmp_7, str) and not isinstance(tmp_7, unicode):
                raise TypeError("string", repr(tmp_7))
        if not isinstance(results['keys'], dict):
            raise TypeError("(string * string) list", repr(results['keys']))
        for tmp_8 in results['keys'].keys():
            if not isinstance(tmp_8, str) and not isinstance(tmp_8, unicode):
                raise TypeError("string", repr(tmp_8))
        for tmp_8 in results['keys'].values():
            if not isinstance(tmp_8, str) and not isinstance(tmp_8, unicode):
                raise TypeError("string", repr(tmp_8))
        return results
    def clone(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        results = self._impl.clone(dbg, sr, key)
        if not isinstance(results['key'], str) and not isinstance(results['key'], unicode):
            raise TypeError("string", repr(results['key']))
        if results['uuid'] is not None:
            if not isinstance(results['uuid'], str) and not isinstance(results['uuid'], unicode):
                raise TypeError("string", repr(results['uuid']))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise TypeError("string", repr(results['name']))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise TypeError("string", repr(results['description']))
        if not isinstance(results['read_write'], bool):
            raise TypeError("bool", repr(results['read_write']))
        if not(is_long(results['virtual_size'])):
            raise TypeError("int64", repr(results['virtual_size']))
        if not(is_long(results['physical_utilisation'])):
            raise TypeError("int64", repr(results['physical_utilisation']))
        if not isinstance(results['uri'], list):
            raise TypeError("string list", repr(results['uri']))
        for tmp_9 in results['uri']:
            if not isinstance(tmp_9, str) and not isinstance(tmp_9, unicode):
                raise TypeError("string", repr(tmp_9))
        if not isinstance(results['keys'], dict):
            raise TypeError("(string * string) list", repr(results['keys']))
        for tmp_10 in results['keys'].keys():
            if not isinstance(tmp_10, str) and not isinstance(tmp_10, unicode):
                raise TypeError("string", repr(tmp_10))
        for tmp_10 in results['keys'].values():
            if not isinstance(tmp_10, str) and not isinstance(tmp_10, unicode):
                raise TypeError("string", repr(tmp_10))
        return results
    def destroy(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        results = self._impl.destroy(dbg, sr, key)
        return results
    def set_name(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        if not('new_name' in args):
            raise UnmarshalException('argument missing', 'new_name', '')
        new_name = args["new_name"]
        if not isinstance(new_name, str) and not isinstance(new_name, unicode):
            raise TypeError("string", repr(new_name))
        results = self._impl.set_name(dbg, sr, key, new_name)
        return results
    def set_description(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        if not('new_description' in args):
            raise UnmarshalException('argument missing', 'new_description', '')
        new_description = args["new_description"]
        if not isinstance(new_description, str) and not isinstance(new_description, unicode):
            raise TypeError("string", repr(new_description))
        results = self._impl.set_description(dbg, sr, key, new_description)
        return results
    def set(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        if not('k' in args):
            raise UnmarshalException('argument missing', 'k', '')
        k = args["k"]
        if not isinstance(k, str) and not isinstance(k, unicode):
            raise TypeError("string", repr(k))
        if not('v' in args):
            raise UnmarshalException('argument missing', 'v', '')
        v = args["v"]
        if not isinstance(v, str) and not isinstance(v, unicode):
            raise TypeError("string", repr(v))
        results = self._impl.set(dbg, sr, key, k, v)
        return results
    def unset(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        if not('k' in args):
            raise UnmarshalException('argument missing', 'k', '')
        k = args["k"]
        if not isinstance(k, str) and not isinstance(k, unicode):
            raise TypeError("string", repr(k))
        results = self._impl.unset(dbg, sr, key, k)
        return results
    def resize(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        if not('new_size' in args):
            raise UnmarshalException('argument missing', 'new_size', '')
        new_size = args["new_size"]
        if not(is_long(new_size)):
            raise TypeError("int64", repr(new_size))
        results = self._impl.resize(dbg, sr, key, new_size)
        return results
    def stat(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('key' in args):
            raise UnmarshalException('argument missing', 'key', '')
        key = args["key"]
        if not isinstance(key, str) and not isinstance(key, unicode):
            raise TypeError("string", repr(key))
        results = self._impl.stat(dbg, sr, key)
        if not isinstance(results['key'], str) and not isinstance(results['key'], unicode):
            raise TypeError("string", repr(results['key']))
        if results['uuid'] is not None:
            if not isinstance(results['uuid'], str) and not isinstance(results['uuid'], unicode):
                raise TypeError("string", repr(results['uuid']))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise TypeError("string", repr(results['name']))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise TypeError("string", repr(results['description']))
        if not isinstance(results['read_write'], bool):
            raise TypeError("bool", repr(results['read_write']))
        if not(is_long(results['virtual_size'])):
            raise TypeError("int64", repr(results['virtual_size']))
        if not(is_long(results['physical_utilisation'])):
            raise TypeError("int64", repr(results['physical_utilisation']))
        if not isinstance(results['uri'], list):
            raise TypeError("string list", repr(results['uri']))
        for tmp_11 in results['uri']:
            if not isinstance(tmp_11, str) and not isinstance(tmp_11, unicode):
                raise TypeError("string", repr(tmp_11))
        if not isinstance(results['keys'], dict):
            raise TypeError("(string * string) list", repr(results['keys']))
        for tmp_12 in results['keys'].keys():
            if not isinstance(tmp_12, str) and not isinstance(tmp_12, unicode):
                raise TypeError("string", repr(tmp_12))
        for tmp_12 in results['keys'].values():
            if not isinstance(tmp_12, str) and not isinstance(tmp_12, unicode):
                raise TypeError("string", repr(tmp_12))
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
        elif method == "Volume.set_name":
            return success(self.set_name(args))
        elif method == "Volume.set_description":
            return success(self.set_description(args))
        elif method == "Volume.set":
            return success(self.set(args))
        elif method == "Volume.unset":
            return success(self.unset(args))
        elif method == "Volume.resize":
            return success(self.resize(args))
        elif method == "Volume.stat":
            return success(self.stat(args))
class Volume_skeleton:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self):
        pass
    def create(self, dbg, sr, name, description, size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.create")
    def snapshot(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.snapshot")
    def clone(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.clone")
    def destroy(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.destroy")
    def set_name(self, dbg, sr, key, new_name):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.set_name")
    def set_description(self, dbg, sr, key, new_description):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.set_description")
    def set(self, dbg, sr, key, k, v):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.set")
    def unset(self, dbg, sr, key, k):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.unset")
    def resize(self, dbg, sr, key, new_size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.resize")
    def stat(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        raise Unimplemented("Volume.stat")
class Volume_test:
    """Operations which operate on volumes (also known as Virtual Disk Images)"""
    def __init__(self):
        pass
    def create(self, dbg, sr, name, description, size):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["volume"] = { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } }
        return result
    def snapshot(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["volume"] = { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } }
        return result
    def clone(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        result["volume"] = { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } }
        return result
    def destroy(self, dbg, sr, key):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def set_name(self, dbg, sr, key, new_name):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def set_description(self, dbg, sr, key, new_description):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def set(self, dbg, sr, key, k, v):
        """Operations which operate on volumes (also known as Virtual Disk Images)"""
        result = {}
        return result
    def unset(self, dbg, sr, key, k):
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
        result["volume"] = { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } }
        return result
class Volume_commandline():
    """Parse command-line arguments and call an implementation."""
    def __init__(self, impl):
        self.impl = impl
        self.dispatcher = Volume_server_dispatcher(self.impl)
    def _parse_create(self):
        """[create sr name description size] creates a new volume in [sr] with [name] and [description]. The volume will have size >= [size] i.e. it is always permissable for an implementation to round-up the volume to the nearest convenient block size"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[create sr name description size] creates a new volume in [sr] with [name] and [description]. The volume will have size >= [size] i.e. it is always permissable for an implementation to round-up the volume to the nearest convenient block size')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('name', action='store', help='A human-readable name to associate with the new disk. This name is intended to be short, to be a good summary of the disk.')
        parser.add_argument('description', action='store', help='A human-readable description to associate with the new disk. This can be arbitrarily long, up to the general string size limit.')
        parser.add_argument('size', action='store', help='A minimum size (in bytes) for the disk. Depending on the characteristics of the implementation this may be rounded up to (for example) the nearest convenient block size. The created disk will not be smaller than this size.')
        return vars(parser.parse_args())
    def _parse_snapshot(self):
        """[snapshot sr volume] creates a new volue which is a  snapshot of [volume] in [sr]. Snapshots should never be written to; they are intended for backup/restore only. Note the name and description are copied but any extra metadata associated by [set] is not copied."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[snapshot sr volume] creates a new volue which is a  snapshot of [volume] in [sr]. Snapshots should never be written to; they are intended for backup/restore only. Note the name and description are copied but any extra metadata associated by [set] is not copied.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        return vars(parser.parse_args())
    def _parse_clone(self):
        """[clone sr volume] creates a new volume which is a writable clone of [volume] in [sr]. Note the name and description are copied but any extra metadata associated by [set] is not copied."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[clone sr volume] creates a new volume which is a writable clone of [volume] in [sr]. Note the name and description are copied but any extra metadata associated by [set] is not copied.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        return vars(parser.parse_args())
    def _parse_destroy(self):
        """[destroy sr volume] removes [volume] from [sr]"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[destroy sr volume] removes [volume] from [sr]')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        return vars(parser.parse_args())
    def _parse_set_name(self):
        """[set_name sr volume new_name] changes the name of [volume]"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[set_name sr volume new_name] changes the name of [volume]')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        parser.add_argument('new_name', action='store', help='New name')
        return vars(parser.parse_args())
    def _parse_set_description(self):
        """[set_description sr volume new_description] changes the description of [volume]"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[set_description sr volume new_description] changes the description of [volume]')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        parser.add_argument('new_description', action='store', help='New description')
        return vars(parser.parse_args())
    def _parse_set(self):
        """[set sr volume key value] associates [key] with [value] in the metadata of [volume] Note these keys and values are not interpreted by the plugin; they are intended for the higher-level software only."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[set sr volume key value] associates [key] with [value] in the metadata of [volume] Note these keys and values are not interpreted by the plugin; they are intended for the higher-level software only.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        parser.add_argument('k', action='store', help='Key')
        parser.add_argument('v', action='store', help='Value')
        return vars(parser.parse_args())
    def _parse_unset(self):
        """[unset sr volume key] removes [key] and any value associated with it from the metadata of [volume] Note these keys and values are not interpreted by the plugin; they are intended for the higher-level software only."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[unset sr volume key] removes [key] and any value associated with it from the metadata of [volume] Note these keys and values are not interpreted by the plugin; they are intended for the higher-level software only.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        parser.add_argument('k', action='store', help='Key')
        return vars(parser.parse_args())
    def _parse_resize(self):
        """[resize sr volume new_size] enlarges [volume] to be at least [new_size]."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[resize sr volume new_size] enlarges [volume] to be at least [new_size].')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        parser.add_argument('new_size', action='store', help='New disk size')
        return vars(parser.parse_args())
    def _parse_stat(self):
        """[stat sr volume] returns metadata associated with [volume]."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[stat sr volume] returns metadata associated with [volume].')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('key', action='store', help='The volume key')
        return vars(parser.parse_args())
    def create(self):
        use_json = False
        try:
            request = self._parse_create()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.create(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def snapshot(self):
        use_json = False
        try:
            request = self._parse_snapshot()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.snapshot(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def clone(self):
        use_json = False
        try:
            request = self._parse_clone()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.clone(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def destroy(self):
        use_json = False
        try:
            request = self._parse_destroy()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.destroy(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def set_name(self):
        use_json = False
        try:
            request = self._parse_set_name()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.set_name(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def set_description(self):
        use_json = False
        try:
            request = self._parse_set_description()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.set_description(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def set(self):
        use_json = False
        try:
            request = self._parse_set()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.set(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def unset(self):
        use_json = False
        try:
            request = self._parse_unset()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.unset(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def resize(self):
        use_json = False
        try:
            request = self._parse_resize()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.resize(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def stat(self):
        use_json = False
        try:
            request = self._parse_stat()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.stat(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
class SR_server_dispatcher:
    """Operations which act on Storage Repositories"""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def probe(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('uri' in args):
            raise UnmarshalException('argument missing', 'uri', '')
        uri = args["uri"]
        if not isinstance(uri, str) and not isinstance(uri, unicode):
            raise TypeError("string", repr(uri))
        results = self._impl.probe(dbg, uri)
        if not isinstance(results['srs'], list):
            raise TypeError("7 list", repr(results['srs']))
        for tmp_13 in results['srs']:
            if not isinstance(tmp_13['sr'], str) and not isinstance(tmp_13['sr'], unicode):
                raise TypeError("string", repr(tmp_13['sr']))
            if not isinstance(tmp_13['name'], str) and not isinstance(tmp_13['name'], unicode):
                raise TypeError("string", repr(tmp_13['name']))
            if not isinstance(tmp_13['description'], str) and not isinstance(tmp_13['description'], unicode):
                raise TypeError("string", repr(tmp_13['description']))
            if not(is_long(tmp_13['free_space'])):
                raise TypeError("int64", repr(tmp_13['free_space']))
            if not(is_long(tmp_13['total_space'])):
                raise TypeError("int64", repr(tmp_13['total_space']))
            if not isinstance(tmp_13['datasources'], list):
                raise TypeError("string list", repr(tmp_13['datasources']))
            for tmp_14 in tmp_13['datasources']:
                if not isinstance(tmp_14, str) and not isinstance(tmp_14, unicode):
                    raise TypeError("string", repr(tmp_14))
            if not isinstance(tmp_13['clustered'], bool):
                raise TypeError("bool", repr(tmp_13['clustered']))
            if tmp_13['health'][0] == 'Healthy':
                if not isinstance(tmp_13['health'][1], str) and not isinstance(tmp_13['health'][1], unicode):
                    raise TypeError("string", repr(tmp_13['health'][1]))
            elif tmp_13['health'][0] == 'Recovering':
                if not isinstance(tmp_13['health'][1], str) and not isinstance(tmp_13['health'][1], unicode):
                    raise TypeError("string", repr(tmp_13['health'][1]))
        if not isinstance(results['uris'], list):
            raise TypeError("string list", repr(results['uris']))
        for tmp_15 in results['uris']:
            if not isinstance(tmp_15, str) and not isinstance(tmp_15, unicode):
                raise TypeError("string", repr(tmp_15))
        return results
    def create(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('uri' in args):
            raise UnmarshalException('argument missing', 'uri', '')
        uri = args["uri"]
        if not isinstance(uri, str) and not isinstance(uri, unicode):
            raise TypeError("string", repr(uri))
        if not('name' in args):
            raise UnmarshalException('argument missing', 'name', '')
        name = args["name"]
        if not isinstance(name, str) and not isinstance(name, unicode):
            raise TypeError("string", repr(name))
        if not('description' in args):
            raise UnmarshalException('argument missing', 'description', '')
        description = args["description"]
        if not isinstance(description, str) and not isinstance(description, unicode):
            raise TypeError("string", repr(description))
        if not('configuration' in args):
            raise UnmarshalException('argument missing', 'configuration', '')
        configuration = args["configuration"]
        if not isinstance(configuration, dict):
            raise TypeError("(string * string) list", repr(configuration))
        for tmp_16 in configuration.keys():
            if not isinstance(tmp_16, str) and not isinstance(tmp_16, unicode):
                raise TypeError("string", repr(tmp_16))
        for tmp_16 in configuration.values():
            if not isinstance(tmp_16, str) and not isinstance(tmp_16, unicode):
                raise TypeError("string", repr(tmp_16))
        results = self._impl.create(dbg, uri, name, description, configuration)
        return results
    def attach(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('uri' in args):
            raise UnmarshalException('argument missing', 'uri', '')
        uri = args["uri"]
        if not isinstance(uri, str) and not isinstance(uri, unicode):
            raise TypeError("string", repr(uri))
        results = self._impl.attach(dbg, uri)
        if not isinstance(results, str) and not isinstance(results, unicode):
            raise TypeError("string", repr(results))
        return results
    def detach(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        results = self._impl.detach(dbg, sr)
        return results
    def destroy(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        results = self._impl.destroy(dbg, sr)
        return results
    def stat(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        results = self._impl.stat(dbg, sr)
        if not isinstance(results['sr'], str) and not isinstance(results['sr'], unicode):
            raise TypeError("string", repr(results['sr']))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise TypeError("string", repr(results['name']))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise TypeError("string", repr(results['description']))
        if not(is_long(results['free_space'])):
            raise TypeError("int64", repr(results['free_space']))
        if not(is_long(results['total_space'])):
            raise TypeError("int64", repr(results['total_space']))
        if not isinstance(results['datasources'], list):
            raise TypeError("string list", repr(results['datasources']))
        for tmp_17 in results['datasources']:
            if not isinstance(tmp_17, str) and not isinstance(tmp_17, unicode):
                raise TypeError("string", repr(tmp_17))
        if not isinstance(results['clustered'], bool):
            raise TypeError("bool", repr(results['clustered']))
        if results['health'][0] == 'Healthy':
            if not isinstance(results['health'][1], str) and not isinstance(results['health'][1], unicode):
                raise TypeError("string", repr(results['health'][1]))
        elif results['health'][0] == 'Recovering':
            if not isinstance(results['health'][1], str) and not isinstance(results['health'][1], unicode):
                raise TypeError("string", repr(results['health'][1]))
        return results
    def set_name(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('new_name' in args):
            raise UnmarshalException('argument missing', 'new_name', '')
        new_name = args["new_name"]
        if not isinstance(new_name, str) and not isinstance(new_name, unicode):
            raise TypeError("string", repr(new_name))
        results = self._impl.set_name(dbg, sr, new_name)
        return results
    def set_description(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        if not('new_description' in args):
            raise UnmarshalException('argument missing', 'new_description', '')
        new_description = args["new_description"]
        if not isinstance(new_description, str) and not isinstance(new_description, unicode):
            raise TypeError("string", repr(new_description))
        results = self._impl.set_description(dbg, sr, new_description)
        return results
    def ls(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise UnmarshalException('arguments', 'dict', repr(args))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise TypeError("string", repr(dbg))
        if not('sr' in args):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if not isinstance(sr, str) and not isinstance(sr, unicode):
            raise TypeError("string", repr(sr))
        results = self._impl.ls(dbg, sr)
        if not isinstance(results, list):
            raise TypeError("8 list", repr(results))
        for tmp_18 in results:
            if not isinstance(tmp_18['key'], str) and not isinstance(tmp_18['key'], unicode):
                raise TypeError("string", repr(tmp_18['key']))
            if tmp_18['uuid'] is not None:
                if not isinstance(tmp_18['uuid'], str) and not isinstance(tmp_18['uuid'], unicode):
                    raise TypeError("string", repr(tmp_18['uuid']))
            if not isinstance(tmp_18['name'], str) and not isinstance(tmp_18['name'], unicode):
                raise TypeError("string", repr(tmp_18['name']))
            if not isinstance(tmp_18['description'], str) and not isinstance(tmp_18['description'], unicode):
                raise TypeError("string", repr(tmp_18['description']))
            if not isinstance(tmp_18['read_write'], bool):
                raise TypeError("bool", repr(tmp_18['read_write']))
            if not(is_long(tmp_18['virtual_size'])):
                raise TypeError("int64", repr(tmp_18['virtual_size']))
            if not(is_long(tmp_18['physical_utilisation'])):
                raise TypeError("int64", repr(tmp_18['physical_utilisation']))
            if not isinstance(tmp_18['uri'], list):
                raise TypeError("string list", repr(tmp_18['uri']))
            for tmp_19 in tmp_18['uri']:
                if not isinstance(tmp_19, str) and not isinstance(tmp_19, unicode):
                    raise TypeError("string", repr(tmp_19))
            if not isinstance(tmp_18['keys'], dict):
                raise TypeError("(string * string) list", repr(tmp_18['keys']))
            for tmp_20 in tmp_18['keys'].keys():
                if not isinstance(tmp_20, str) and not isinstance(tmp_20, unicode):
                    raise TypeError("string", repr(tmp_20))
            for tmp_20 in tmp_18['keys'].values():
                if not isinstance(tmp_20, str) and not isinstance(tmp_20, unicode):
                    raise TypeError("string", repr(tmp_20))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "SR.probe":
            return success(self.probe(args))
        elif method == "SR.create":
            return success(self.create(args))
        elif method == "SR.attach":
            return success(self.attach(args))
        elif method == "SR.detach":
            return success(self.detach(args))
        elif method == "SR.destroy":
            return success(self.destroy(args))
        elif method == "SR.stat":
            return success(self.stat(args))
        elif method == "SR.set_name":
            return success(self.set_name(args))
        elif method == "SR.set_description":
            return success(self.set_description(args))
        elif method == "SR.ls":
            return success(self.ls(args))
class SR_skeleton:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def probe(self, dbg, uri):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.probe")
    def create(self, dbg, uri, name, description, configuration):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.create")
    def attach(self, dbg, uri):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.attach")
    def detach(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.detach")
    def destroy(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.destroy")
    def stat(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.stat")
    def set_name(self, dbg, sr, new_name):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.set_name")
    def set_description(self, dbg, sr, new_description):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.set_description")
    def ls(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.ls")
class SR_test:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def probe(self, dbg, uri):
        """Operations which act on Storage Repositories"""
        result = {}
        result["result"] = { "srs": [ { "sr": "string", "name": "string", "description": "string", "free_space": 0L, "total_space": 0L, "datasources": [ "string", "string" ], "clustered": True, "health": None }, { "sr": "string", "name": "string", "description": "string", "free_space": 0L, "total_space": 0L, "datasources": [ "string", "string" ], "clustered": True, "health": None } ], "uris": [ "string", "string" ] }
        return result
    def create(self, dbg, uri, name, description, configuration):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def attach(self, dbg, uri):
        """Operations which act on Storage Repositories"""
        result = {}
        result["sr"] = "string"
        return result
    def detach(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def destroy(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def stat(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        result["sr"] = { "sr": "string", "name": "string", "description": "string", "free_space": 0L, "total_space": 0L, "datasources": [ "string", "string" ], "clustered": True, "health": None }
        return result
    def set_name(self, dbg, sr, new_name):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def set_description(self, dbg, sr, new_description):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def ls(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        result["volumes"] = [ { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } }, { "key": "string", "uuid": None, "name": "string", "description": "string", "read_write": True, "virtual_size": 0L, "physical_utilisation": 0L, "uri": [ "string", "string" ], "keys": { "string": "string" } } ]
        return result
class SR_commandline():
    """Parse command-line arguments and call an implementation."""
    def __init__(self, impl):
        self.impl = impl
        self.dispatcher = SR_server_dispatcher(self.impl)
    def _parse_probe(self):
        """[probe uri]: looks for existing SRs on the storage device"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[probe uri]: looks for existing SRs on the storage device')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('uri', action='store', help='The Storage Repository URI')
        return vars(parser.parse_args())
    def _parse_create(self):
        """[create uri name description configuration]: creates a fresh SR"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[create uri name description configuration]: creates a fresh SR')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('uri', action='store', help='The Storage Repository URI')
        parser.add_argument('name', action='store', help='Human-readable name for the SR')
        parser.add_argument('description', action='store', help='Human-readable description for the SR')
        parser.add_argument('--configuration', default={}, nargs=2, action=xapi.ListAction, help='Plugin-specific configuration which describes where and how to create the storage repository. This may include the physical block device name, a remote NFS server and path or an RBD storage pool.')
        return vars(parser.parse_args())
    def _parse_attach(self):
        """[attach uri]: attaches the SR to the local host. Once an SR is attached then volumes may be manipulated."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[attach uri]: attaches the SR to the local host. Once an SR is attached then volumes may be manipulated.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('uri', action='store', help='The Storage Repository URI')
        return vars(parser.parse_args())
    def _parse_detach(self):
        """[detach sr]: detaches the SR, clearing up any associated resources. Once the SR is detached then volumes may not be manipulated."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[detach sr]: detaches the SR, clearing up any associated resources. Once the SR is detached then volumes may not be manipulated.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        return vars(parser.parse_args())
    def _parse_destroy(self):
        """[destroy sr]: destroys the [sr] and deletes any volumes associated with it. Note that an SR must be attached to be destroyed; otherwise Sr_not_attached is thrown."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[destroy sr]: destroys the [sr] and deletes any volumes associated with it. Note that an SR must be attached to be destroyed; otherwise Sr_not_attached is thrown.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        return vars(parser.parse_args())
    def _parse_stat(self):
        """[stat sr] returns summary metadata associated with [sr]. Note this call does not return details of sub-volumes, see SR.ls."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[stat sr] returns summary metadata associated with [sr]. Note this call does not return details of sub-volumes, see SR.ls.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        return vars(parser.parse_args())
    def _parse_set_name(self):
        """[set_name sr new_name] changes the name of [sr]"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[set_name sr new_name] changes the name of [sr]')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('new_name', action='store', help='The new name of the SR')
        return vars(parser.parse_args())
    def _parse_set_description(self):
        """[set_description sr new_description] changes the description of [sr]"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[set_description sr new_description] changes the description of [sr]')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        parser.add_argument('new_description', action='store', help='The new description for the SR')
        return vars(parser.parse_args())
    def _parse_ls(self):
        """[ls sr] returns a list of volumes contained within an attached SR."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[ls sr] returns a list of volumes contained within an attached SR.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        parser.add_argument('sr', action='store', help='The Storage Repository')
        return vars(parser.parse_args())
    def probe(self):
        use_json = False
        try:
            request = self._parse_probe()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.probe(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def create(self):
        use_json = False
        try:
            request = self._parse_create()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.create(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def attach(self):
        use_json = False
        try:
            request = self._parse_attach()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.attach(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def detach(self):
        use_json = False
        try:
            request = self._parse_detach()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.detach(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def destroy(self):
        use_json = False
        try:
            request = self._parse_destroy()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.destroy(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def stat(self):
        use_json = False
        try:
            request = self._parse_stat()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.stat(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def set_name(self):
        use_json = False
        try:
            request = self._parse_set_name()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.set_name(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def set_description(self):
        use_json = False
        try:
            request = self._parse_set_description()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.set_description(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
    def ls(self):
        use_json = False
        try:
            request = self._parse_ls()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.ls(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
class volume_server_dispatcher:
    """Demux calls to individual interface server_dispatchers"""
    def __init__(self, Volume=None, SR=None):
        self.Volume = Volume
        self.SR = SR
    def _dispatch(self, method, params):
        try:
            logging.debug("method = %s params = %s" % (method, repr(params)))
            if method.startswith("Volume") and self.Volume:
                return self.Volume._dispatch(method, params)
            elif method.startswith("SR") and self.SR:
                return self.SR._dispatch(method, params)
            raise UnknownMethod(method)
        except Exception, e:
            logging.info("caught %s" % e)
            traceback.print_exc()
            try:
                # A declared (expected) failure will have a .failure() method
                logging.debug("returning %s" % (repr(e.failure())))
                return e.failure()
            except AttributeError:
                # An undeclared (unexpected) failure is wrapped as InternalError
                return (InternalError(str(e)).failure())
class volume_server_test(volume_server_dispatcher):
    """Create a server which will respond to all calls, returning arbitrary values. This is intended as a marshal/unmarshal test."""
    def __init__(self):
        volume_server_dispatcher.__init__(self, Volume_server_dispatcher(Volume_test()), SR_server_dispatcher(SR_test()))