from xcp import *
import traceback
class Sr_not_attached(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Sr_not_attached", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Vdi_does_not_exist(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Vdi_does_not_exist", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Illegal_transition(Rpc_light_failure):
    def __init__(self, arg_0, arg_1):
        Rpc_light_failure.__init__(self, "Illegal_transition", [ arg_0, arg_1 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        if type(arg_1) <> type(""):
            raise (TypeError("string", repr(arg_1)))
        self.arg_0 = arg_0
        self.arg_1 = arg_1
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
class Redirect(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Redirect", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Sr_attached(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Sr_attached", [ arg_0 ])
        if type(arg_0) <> type(""):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Query_server_dispatcher:
    """Discover properties of this implementation"""
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
        if method == "Query.query":
            return success(self.query(args))
        elif method == "Query.diagnostics":
            return success(self.diagnostics(args))
class Query_skeleton:
    """Discover properties of this implementation"""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation"""
        raise Unimplemented("Query.query")
    def diagnostics(self, dbg):
        """Discover properties of this implementation"""
        raise Unimplemented("Query.diagnostics")
class Query_test:
    """Discover properties of this implementation"""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation"""
        result = {}
        result["query_result"] = { "driver": "string", "name": "string", "description": "string", "vendor": "string", "copyright": "string", "version": "string", "required_api_version": "string", "features": [ "string", "string" ], "configuration": { "string": "string" } }
        return result
    def diagnostics(self, dbg):
        """Discover properties of this implementation"""
        result = {}
        result["diagnostics"] = "string"
        return result
class VDI_server_dispatcher:
    """Operations which operate on Virtual Disk Images"""
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
        if not(args.has_key('vdi_info')):
            raise UnmarshalException('argument missing', 'vdi_info', '')
        vdi_info = args["vdi_info"]
        if type(vdi_info['vdi']) <> type(""):
            raise (TypeError("string", repr(vdi_info['vdi'])))
        if type(vdi_info['content_id']) <> type(""):
            raise (TypeError("string", repr(vdi_info['content_id'])))
        if type(vdi_info['name_label']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_label'])))
        if type(vdi_info['name_description']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_description'])))
        if type(vdi_info['ty']) <> type(""):
            raise (TypeError("string", repr(vdi_info['ty'])))
        if type(vdi_info['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(vdi_info['metadata_of_pool'])))
        if type(vdi_info['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['is_a_snapshot'])))
        if type(vdi_info['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_time'])))
        if type(vdi_info['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_of'])))
        if type(vdi_info['read_only']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['read_only'])))
        if not(is_long(vdi_info['virtual_size'])):
            raise (TypeError("int64", repr(vdi_info['virtual_size'])))
        if not(is_long(vdi_info['physical_utilisation'])):
            raise (TypeError("int64", repr(vdi_info['physical_utilisation'])))
        if type(vdi_info['persistent']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['persistent'])))
        if type(vdi_info['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(vdi_info['sm_config'])))
        for x in vdi_info['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in vdi_info['sm_config'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        results = self._impl.create(dbg, sr, vdi_info)
        if type(results['vdi']) <> type(""):
            raise (TypeError("string", repr(results['vdi'])))
        if type(results['content_id']) <> type(""):
            raise (TypeError("string", repr(results['content_id'])))
        if type(results['name_label']) <> type(""):
            raise (TypeError("string", repr(results['name_label'])))
        if type(results['name_description']) <> type(""):
            raise (TypeError("string", repr(results['name_description'])))
        if type(results['ty']) <> type(""):
            raise (TypeError("string", repr(results['ty'])))
        if type(results['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(results['metadata_of_pool'])))
        if type(results['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(results['is_a_snapshot'])))
        if type(results['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_time'])))
        if type(results['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_of'])))
        if type(results['read_only']) <> type(True):
            raise (TypeError("bool", repr(results['read_only'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if not(is_long(results['physical_utilisation'])):
            raise (TypeError("int64", repr(results['physical_utilisation'])))
        if type(results['persistent']) <> type(True):
            raise (TypeError("bool", repr(results['persistent'])))
        if type(results['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['sm_config'])))
        for x in results['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['sm_config'].values():
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
        if not(args.has_key('vdi_info')):
            raise UnmarshalException('argument missing', 'vdi_info', '')
        vdi_info = args["vdi_info"]
        if type(vdi_info['vdi']) <> type(""):
            raise (TypeError("string", repr(vdi_info['vdi'])))
        if type(vdi_info['content_id']) <> type(""):
            raise (TypeError("string", repr(vdi_info['content_id'])))
        if type(vdi_info['name_label']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_label'])))
        if type(vdi_info['name_description']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_description'])))
        if type(vdi_info['ty']) <> type(""):
            raise (TypeError("string", repr(vdi_info['ty'])))
        if type(vdi_info['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(vdi_info['metadata_of_pool'])))
        if type(vdi_info['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['is_a_snapshot'])))
        if type(vdi_info['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_time'])))
        if type(vdi_info['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_of'])))
        if type(vdi_info['read_only']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['read_only'])))
        if not(is_long(vdi_info['virtual_size'])):
            raise (TypeError("int64", repr(vdi_info['virtual_size'])))
        if not(is_long(vdi_info['physical_utilisation'])):
            raise (TypeError("int64", repr(vdi_info['physical_utilisation'])))
        if type(vdi_info['persistent']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['persistent'])))
        if type(vdi_info['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(vdi_info['sm_config'])))
        for x in vdi_info['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in vdi_info['sm_config'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        results = self._impl.snapshot(dbg, sr, vdi_info)
        if type(results['vdi']) <> type(""):
            raise (TypeError("string", repr(results['vdi'])))
        if type(results['content_id']) <> type(""):
            raise (TypeError("string", repr(results['content_id'])))
        if type(results['name_label']) <> type(""):
            raise (TypeError("string", repr(results['name_label'])))
        if type(results['name_description']) <> type(""):
            raise (TypeError("string", repr(results['name_description'])))
        if type(results['ty']) <> type(""):
            raise (TypeError("string", repr(results['ty'])))
        if type(results['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(results['metadata_of_pool'])))
        if type(results['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(results['is_a_snapshot'])))
        if type(results['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_time'])))
        if type(results['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_of'])))
        if type(results['read_only']) <> type(True):
            raise (TypeError("bool", repr(results['read_only'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if not(is_long(results['physical_utilisation'])):
            raise (TypeError("int64", repr(results['physical_utilisation'])))
        if type(results['persistent']) <> type(True):
            raise (TypeError("bool", repr(results['persistent'])))
        if type(results['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['sm_config'])))
        for x in results['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['sm_config'].values():
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
        if not(args.has_key('vdi_info')):
            raise UnmarshalException('argument missing', 'vdi_info', '')
        vdi_info = args["vdi_info"]
        if type(vdi_info['vdi']) <> type(""):
            raise (TypeError("string", repr(vdi_info['vdi'])))
        if type(vdi_info['content_id']) <> type(""):
            raise (TypeError("string", repr(vdi_info['content_id'])))
        if type(vdi_info['name_label']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_label'])))
        if type(vdi_info['name_description']) <> type(""):
            raise (TypeError("string", repr(vdi_info['name_description'])))
        if type(vdi_info['ty']) <> type(""):
            raise (TypeError("string", repr(vdi_info['ty'])))
        if type(vdi_info['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(vdi_info['metadata_of_pool'])))
        if type(vdi_info['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['is_a_snapshot'])))
        if type(vdi_info['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_time'])))
        if type(vdi_info['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(vdi_info['snapshot_of'])))
        if type(vdi_info['read_only']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['read_only'])))
        if not(is_long(vdi_info['virtual_size'])):
            raise (TypeError("int64", repr(vdi_info['virtual_size'])))
        if not(is_long(vdi_info['physical_utilisation'])):
            raise (TypeError("int64", repr(vdi_info['physical_utilisation'])))
        if type(vdi_info['persistent']) <> type(True):
            raise (TypeError("bool", repr(vdi_info['persistent'])))
        if type(vdi_info['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(vdi_info['sm_config'])))
        for x in vdi_info['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in vdi_info['sm_config'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        results = self._impl.clone(dbg, sr, vdi_info)
        if type(results['vdi']) <> type(""):
            raise (TypeError("string", repr(results['vdi'])))
        if type(results['content_id']) <> type(""):
            raise (TypeError("string", repr(results['content_id'])))
        if type(results['name_label']) <> type(""):
            raise (TypeError("string", repr(results['name_label'])))
        if type(results['name_description']) <> type(""):
            raise (TypeError("string", repr(results['name_description'])))
        if type(results['ty']) <> type(""):
            raise (TypeError("string", repr(results['ty'])))
        if type(results['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(results['metadata_of_pool'])))
        if type(results['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(results['is_a_snapshot'])))
        if type(results['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_time'])))
        if type(results['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_of'])))
        if type(results['read_only']) <> type(True):
            raise (TypeError("bool", repr(results['read_only'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if not(is_long(results['physical_utilisation'])):
            raise (TypeError("int64", repr(results['physical_utilisation'])))
        if type(results['persistent']) <> type(True):
            raise (TypeError("bool", repr(results['persistent'])))
        if type(results['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['sm_config'])))
        for x in results['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['sm_config'].values():
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.destroy(dbg, sr, vdi)
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.stat(dbg, sr, vdi)
        if type(results['vdi']) <> type(""):
            raise (TypeError("string", repr(results['vdi'])))
        if type(results['content_id']) <> type(""):
            raise (TypeError("string", repr(results['content_id'])))
        if type(results['name_label']) <> type(""):
            raise (TypeError("string", repr(results['name_label'])))
        if type(results['name_description']) <> type(""):
            raise (TypeError("string", repr(results['name_description'])))
        if type(results['ty']) <> type(""):
            raise (TypeError("string", repr(results['ty'])))
        if type(results['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(results['metadata_of_pool'])))
        if type(results['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(results['is_a_snapshot'])))
        if type(results['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_time'])))
        if type(results['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_of'])))
        if type(results['read_only']) <> type(True):
            raise (TypeError("bool", repr(results['read_only'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if not(is_long(results['physical_utilisation'])):
            raise (TypeError("int64", repr(results['physical_utilisation'])))
        if type(results['persistent']) <> type(True):
            raise (TypeError("bool", repr(results['persistent'])))
        if type(results['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['sm_config'])))
        for x in results['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['sm_config'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def set_persistent(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        if not(args.has_key('persistent')):
            raise UnmarshalException('argument missing', 'persistent', '')
        persistent = args["persistent"]
        if type(persistent) <> type(True):
            raise (TypeError("bool", repr(persistent)))
        results = self._impl.set_persistent(dbg, sr, vdi, persistent)
        return results
    def epoch_begin(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.epoch_begin(dbg, sr, vdi)
        return results
    def epoch_end(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.epoch_end(dbg, sr, vdi)
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
        if not(args.has_key('dp')):
            raise UnmarshalException('argument missing', 'dp', '')
        dp = args["dp"]
        if type(dp) <> type(""):
            raise (TypeError("string", repr(dp)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        if not(args.has_key('read_write')):
            raise UnmarshalException('argument missing', 'read_write', '')
        read_write = args["read_write"]
        if type(read_write) <> type(True):
            raise (TypeError("bool", repr(read_write)))
        results = self._impl.attach(dbg, dp, sr, vdi, read_write)
        if type(results['params']) <> type(""):
            raise (TypeError("string", repr(results['params'])))
        if type(results['xenstore_data']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['xenstore_data'])))
        for x in results['xenstore_data'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['xenstore_data'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def activate(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('dp')):
            raise UnmarshalException('argument missing', 'dp', '')
        dp = args["dp"]
        if type(dp) <> type(""):
            raise (TypeError("string", repr(dp)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.activate(dbg, dp, sr, vdi)
        return results
    def deactivate(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if type(args) <> type({}):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not(args.has_key('dbg')):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if type(dbg) <> type(""):
            raise (TypeError("string", repr(dbg)))
        if not(args.has_key('dp')):
            raise UnmarshalException('argument missing', 'dp', '')
        dp = args["dp"]
        if type(dp) <> type(""):
            raise (TypeError("string", repr(dp)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.deactivate(dbg, dp, sr, vdi)
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
        if not(args.has_key('dp')):
            raise UnmarshalException('argument missing', 'dp', '')
        dp = args["dp"]
        if type(dp) <> type(""):
            raise (TypeError("string", repr(dp)))
        if not(args.has_key('sr')):
            raise UnmarshalException('argument missing', 'sr', '')
        sr = args["sr"]
        if type(sr) <> type(""):
            raise (TypeError("string", repr(sr)))
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.detach(dbg, dp, sr, vdi)
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
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
        results = self._impl.copy(dbg, sr, vdi, url, dest)
        if type(results) <> type(""):
            raise (TypeError("string", repr(results)))
        return results
    def get_url(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.get_url(dbg, sr, vdi)
        if type(results) <> type(""):
            raise (TypeError("string", repr(results)))
        return results
    def get_by_name(self, args):
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
        if not(args.has_key('name')):
            raise UnmarshalException('argument missing', 'name', '')
        name = args["name"]
        if type(name) <> type(""):
            raise (TypeError("string", repr(name)))
        results = self._impl.get_by_name(dbg, sr, name)
        if type(results['vdi']) <> type(""):
            raise (TypeError("string", repr(results['vdi'])))
        if type(results['content_id']) <> type(""):
            raise (TypeError("string", repr(results['content_id'])))
        if type(results['name_label']) <> type(""):
            raise (TypeError("string", repr(results['name_label'])))
        if type(results['name_description']) <> type(""):
            raise (TypeError("string", repr(results['name_description'])))
        if type(results['ty']) <> type(""):
            raise (TypeError("string", repr(results['ty'])))
        if type(results['metadata_of_pool']) <> type(""):
            raise (TypeError("string", repr(results['metadata_of_pool'])))
        if type(results['is_a_snapshot']) <> type(True):
            raise (TypeError("bool", repr(results['is_a_snapshot'])))
        if type(results['snapshot_time']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_time'])))
        if type(results['snapshot_of']) <> type(""):
            raise (TypeError("string", repr(results['snapshot_of'])))
        if type(results['read_only']) <> type(True):
            raise (TypeError("bool", repr(results['read_only'])))
        if not(is_long(results['virtual_size'])):
            raise (TypeError("int64", repr(results['virtual_size'])))
        if not(is_long(results['physical_utilisation'])):
            raise (TypeError("int64", repr(results['physical_utilisation'])))
        if type(results['persistent']) <> type(True):
            raise (TypeError("bool", repr(results['persistent'])))
        if type(results['sm_config']) <> type({}):
            raise (TypeError("(string * string) list", repr(results['sm_config'])))
        for x in results['sm_config'].keys():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        for x in results['sm_config'].values():
            if type(x) <> type(""):
                raise (TypeError("string", repr(x)))
        return results
    def set_content_id(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        if not(args.has_key('content_id')):
            raise UnmarshalException('argument missing', 'content_id', '')
        content_id = args["content_id"]
        if type(content_id) <> type(""):
            raise (TypeError("string", repr(content_id)))
        results = self._impl.set_content_id(dbg, sr, vdi, content_id)
        return results
    def compose(self, args):
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
        if not(args.has_key('vdi1')):
            raise UnmarshalException('argument missing', 'vdi1', '')
        vdi1 = args["vdi1"]
        if type(vdi1) <> type(""):
            raise (TypeError("string", repr(vdi1)))
        if not(args.has_key('vdi2')):
            raise UnmarshalException('argument missing', 'vdi2', '')
        vdi2 = args["vdi2"]
        if type(vdi2) <> type(""):
            raise (TypeError("string", repr(vdi2)))
        results = self._impl.compose(dbg, sr, vdi1, vdi2)
        return results
    def similar_content(self, args):
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
        if not(args.has_key('vdi')):
            raise UnmarshalException('argument missing', 'vdi', '')
        vdi = args["vdi"]
        if type(vdi) <> type(""):
            raise (TypeError("string", repr(vdi)))
        results = self._impl.similar_content(dbg, sr, vdi)
        if type(results) <> type([]):
            raise (TypeError("92 list", repr(results)))
        for x in results:
            if type(x['vdi']) <> type(""):
                raise (TypeError("string", repr(x['vdi'])))
            if type(x['content_id']) <> type(""):
                raise (TypeError("string", repr(x['content_id'])))
            if type(x['name_label']) <> type(""):
                raise (TypeError("string", repr(x['name_label'])))
            if type(x['name_description']) <> type(""):
                raise (TypeError("string", repr(x['name_description'])))
            if type(x['ty']) <> type(""):
                raise (TypeError("string", repr(x['ty'])))
            if type(x['metadata_of_pool']) <> type(""):
                raise (TypeError("string", repr(x['metadata_of_pool'])))
            if type(x['is_a_snapshot']) <> type(True):
                raise (TypeError("bool", repr(x['is_a_snapshot'])))
            if type(x['snapshot_time']) <> type(""):
                raise (TypeError("string", repr(x['snapshot_time'])))
            if type(x['snapshot_of']) <> type(""):
                raise (TypeError("string", repr(x['snapshot_of'])))
            if type(x['read_only']) <> type(True):
                raise (TypeError("bool", repr(x['read_only'])))
            if not(is_long(x['virtual_size'])):
                raise (TypeError("int64", repr(x['virtual_size'])))
            if not(is_long(x['physical_utilisation'])):
                raise (TypeError("int64", repr(x['physical_utilisation'])))
            if type(x['persistent']) <> type(True):
                raise (TypeError("bool", repr(x['persistent'])))
            if type(x['sm_config']) <> type({}):
                raise (TypeError("(string * string) list", repr(x['sm_config'])))
            for x in x['sm_config'].keys():
                if type(x) <> type(""):
                    raise (TypeError("string", repr(x)))
            for x in x['sm_config'].values():
                if type(x) <> type(""):
                    raise (TypeError("string", repr(x)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "VDI.create":
            return success(self.create(args))
        elif method == "VDI.snapshot":
            return success(self.snapshot(args))
        elif method == "VDI.clone":
            return success(self.clone(args))
        elif method == "VDI.destroy":
            return success(self.destroy(args))
        elif method == "VDI.stat":
            return success(self.stat(args))
        elif method == "VDI.set_persistent":
            return success(self.set_persistent(args))
        elif method == "VDI.epoch_begin":
            return success(self.epoch_begin(args))
        elif method == "VDI.epoch_end":
            return success(self.epoch_end(args))
        elif method == "VDI.attach":
            return success(self.attach(args))
        elif method == "VDI.activate":
            return success(self.activate(args))
        elif method == "VDI.deactivate":
            return success(self.deactivate(args))
        elif method == "VDI.detach":
            return success(self.detach(args))
        elif method == "VDI.copy":
            return success(self.copy(args))
        elif method == "VDI.get_url":
            return success(self.get_url(args))
        elif method == "VDI.get_by_name":
            return success(self.get_by_name(args))
        elif method == "VDI.set_content_id":
            return success(self.set_content_id(args))
        elif method == "VDI.compose":
            return success(self.compose(args))
        elif method == "VDI.similar_content":
            return success(self.similar_content(args))
class VDI_skeleton:
    """Operations which operate on Virtual Disk Images"""
    def __init__(self):
        pass
    def create(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.create")
    def snapshot(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.snapshot")
    def clone(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.clone")
    def destroy(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.destroy")
    def stat(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.stat")
    def set_persistent(self, dbg, sr, vdi, persistent):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.set_persistent")
    def epoch_begin(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.epoch_begin")
    def epoch_end(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.epoch_end")
    def attach(self, dbg, dp, sr, vdi, read_write):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.attach")
    def activate(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.activate")
    def deactivate(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.deactivate")
    def detach(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.detach")
    def copy(self, dbg, sr, vdi, url, dest):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.copy")
    def get_url(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.get_url")
    def get_by_name(self, dbg, sr, name):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.get_by_name")
    def set_content_id(self, dbg, sr, vdi, content_id):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.set_content_id")
    def compose(self, dbg, sr, vdi1, vdi2):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.compose")
    def similar_content(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.similar_content")
class VDI_test:
    """Operations which operate on Virtual Disk Images"""
    def __init__(self):
        pass
    def create(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["new_vdi"] = { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }
        return result
    def snapshot(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["new_vdi"] = { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }
        return result
    def clone(self, dbg, sr, vdi_info):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["new_vdi"] = { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }
        return result
    def destroy(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def stat(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["vdi_info"] = { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }
        return result
    def set_persistent(self, dbg, sr, vdi, persistent):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def epoch_begin(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def epoch_end(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def attach(self, dbg, dp, sr, vdi, read_write):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["device"] = { "params": "string", "xenstore_data": { "string": "string" } }
        return result
    def activate(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def deactivate(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def detach(self, dbg, dp, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def copy(self, dbg, sr, vdi, url, dest):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["new_vdi"] = "string"
        return result
    def get_url(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["url"] = "string"
        return result
    def get_by_name(self, dbg, sr, name):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["vdi_info"] = { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }
        return result
    def set_content_id(self, dbg, sr, vdi, content_id):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def compose(self, dbg, sr, vdi1, vdi2):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        return result
    def similar_content(self, dbg, sr, vdi):
        """Operations which operate on Virtual Disk Images"""
        result = {}
        result["vdi_infos"] = [ { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }, { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } } ]
        return result
class SR_server_dispatcher:
    """Operations which act on Storage Repositories"""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
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
        if not(args.has_key('physical_size')):
            raise UnmarshalException('argument missing', 'physical_size', '')
        physical_size = args["physical_size"]
        if not(is_long(physical_size)):
            raise (TypeError("int64", repr(physical_size)))
        results = self._impl.create(dbg, sr, device_config, physical_size)
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
    def reset(self, args):
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
        results = self._impl.reset(dbg, sr)
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
            raise (TypeError("92 list", repr(results)))
        for x in results:
            if type(x['vdi']) <> type(""):
                raise (TypeError("string", repr(x['vdi'])))
            if type(x['content_id']) <> type(""):
                raise (TypeError("string", repr(x['content_id'])))
            if type(x['name_label']) <> type(""):
                raise (TypeError("string", repr(x['name_label'])))
            if type(x['name_description']) <> type(""):
                raise (TypeError("string", repr(x['name_description'])))
            if type(x['ty']) <> type(""):
                raise (TypeError("string", repr(x['ty'])))
            if type(x['metadata_of_pool']) <> type(""):
                raise (TypeError("string", repr(x['metadata_of_pool'])))
            if type(x['is_a_snapshot']) <> type(True):
                raise (TypeError("bool", repr(x['is_a_snapshot'])))
            if type(x['snapshot_time']) <> type(""):
                raise (TypeError("string", repr(x['snapshot_time'])))
            if type(x['snapshot_of']) <> type(""):
                raise (TypeError("string", repr(x['snapshot_of'])))
            if type(x['read_only']) <> type(True):
                raise (TypeError("bool", repr(x['read_only'])))
            if not(is_long(x['virtual_size'])):
                raise (TypeError("int64", repr(x['virtual_size'])))
            if not(is_long(x['physical_utilisation'])):
                raise (TypeError("int64", repr(x['physical_utilisation'])))
            if type(x['persistent']) <> type(True):
                raise (TypeError("bool", repr(x['persistent'])))
            if type(x['sm_config']) <> type({}):
                raise (TypeError("(string * string) list", repr(x['sm_config'])))
            for x in x['sm_config'].keys():
                if type(x) <> type(""):
                    raise (TypeError("string", repr(x)))
            for x in x['sm_config'].values():
                if type(x) <> type(""):
                    raise (TypeError("string", repr(x)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "SR.ls":
            return success(self.ls(args))
        elif method == "SR.create":
            return success(self.create(args))
        elif method == "SR.attach":
            return success(self.attach(args))
        elif method == "SR.detach":
            return success(self.detach(args))
        elif method == "SR.destroy":
            return success(self.destroy(args))
        elif method == "SR.reset":
            return success(self.reset(args))
        elif method == "SR.scan":
            return success(self.scan(args))
class SR_skeleton:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def ls(self, dbg):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.ls")
    def create(self, dbg, sr, device_config, physical_size):
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
    def reset(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.reset")
    def scan(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.scan")
class SR_test:
    """Operations which act on Storage Repositories"""
    def __init__(self):
        pass
    def ls(self, dbg):
        """Operations which act on Storage Repositories"""
        result = {}
        result["srs"] = [ "string", "string" ]
        return result
    def create(self, dbg, sr, device_config, physical_size):
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
    def reset(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        return result
    def scan(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        result = {}
        result["vdis"] = [ { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } }, { "vdi": "string", "content_id": "string", "name_label": "string", "name_description": "string", "ty": "string", "metadata_of_pool": "string", "is_a_snapshot": True, "snapshot_time": "string", "snapshot_of": "string", "read_only": True, "virtual_size": 0L, "physical_utilisation": 0L, "persistent": True, "sm_config": { "string": "string" } } ]
        return result
class storage_server_dispatcher:
    """Demux calls to individual interface server_dispatchers"""
    def __init__(self, Query = None, VDI = None, SR = None):
        self.Query = Query
        self.VDI = VDI
        self.SR = SR
    def _dispatch(self, method, params):
        try:
            log("method = %s params = %s" % (method, repr(params)))
            if method.startswith("Query") and self.Query:
                return self.Query._dispatch(method, params)
            elif method.startswith("VDI") and self.VDI:
                return self.VDI._dispatch(method, params)
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
        storage_server_dispatcher.__init__(self, Query_server_dispatcher(Query_test()), VDI_server_dispatcher(VDI_test()), SR_server_dispatcher(SR_test()))