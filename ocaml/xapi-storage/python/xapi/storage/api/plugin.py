from xapi import success, Rpc_light_failure, InternalError, UnmarshalException, TypeError, is_long, UnknownMethod
import xapi
import sys
import json
import argparse
import traceback
import logging
class Unimplemented(Rpc_light_failure):
    def __init__(self, arg_0):
        Rpc_light_failure.__init__(self, "Unimplemented", [ arg_0 ])
        if not isinstance(arg_0, str) and not isinstance(arg_0, unicode):
            raise (TypeError("string", repr(arg_0)))
        self.arg_0 = arg_0
class Plugin_server_dispatcher:
    """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
    def __init__(self, impl):
        """impl is a proxy object whose methods contain the implementation"""
        self._impl = impl
    def query(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.query(dbg)
        if not isinstance(results['plugin'], str) and not isinstance(results['plugin'], unicode):
            raise (TypeError("string", repr(results['plugin'])))
        if not isinstance(results['name'], str) and not isinstance(results['name'], unicode):
            raise (TypeError("string", repr(results['name'])))
        if not isinstance(results['description'], str) and not isinstance(results['description'], unicode):
            raise (TypeError("string", repr(results['description'])))
        if not isinstance(results['vendor'], str) and not isinstance(results['vendor'], unicode):
            raise (TypeError("string", repr(results['vendor'])))
        if not isinstance(results['copyright'], str) and not isinstance(results['copyright'], unicode):
            raise (TypeError("string", repr(results['copyright'])))
        if not isinstance(results['version'], str) and not isinstance(results['version'], unicode):
            raise (TypeError("string", repr(results['version'])))
        if not isinstance(results['required_api_version'], str) and not isinstance(results['required_api_version'], unicode):
            raise (TypeError("string", repr(results['required_api_version'])))
        if not isinstance(results['features'], list):
            raise (TypeError("string list", repr(results['features'])))
        for tmp_1 in results['features']:
            if not isinstance(tmp_1, str) and not isinstance(tmp_1, unicode):
                raise (TypeError("string", repr(tmp_1)))
        if not isinstance(results['configuration'], dict):
            raise (TypeError("(string * string) list", repr(results['configuration'])))
        for tmp_2 in results['configuration'].keys():
            if not isinstance(tmp_2, str) and not isinstance(tmp_2, unicode):
                raise (TypeError("string", repr(tmp_2)))
        for tmp_2 in results['configuration'].values():
            if not isinstance(tmp_2, str) and not isinstance(tmp_2, unicode):
                raise (TypeError("string", repr(tmp_2)))
        if not isinstance(results['required_cluster_stack'], list):
            raise (TypeError("string list", repr(results['required_cluster_stack'])))
        for tmp_3 in results['required_cluster_stack']:
            if not isinstance(tmp_3, str) and not isinstance(tmp_3, unicode):
                raise (TypeError("string", repr(tmp_3)))
        return results
    def ls(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.ls(dbg)
        if not isinstance(results, list):
            raise (TypeError("string list", repr(results)))
        for tmp_4 in results:
            if not isinstance(tmp_4, str) and not isinstance(tmp_4, unicode):
                raise (TypeError("string", repr(tmp_4)))
        return results
    def diagnostics(self, args):
        """type-check inputs, call implementation, type-check outputs and return"""
        if not isinstance(args, dict):
            raise (UnmarshalException('arguments', 'dict', repr(args)))
        if not('dbg' in args):
            raise UnmarshalException('argument missing', 'dbg', '')
        dbg = args["dbg"]
        if not isinstance(dbg, str) and not isinstance(dbg, unicode):
            raise (TypeError("string", repr(dbg)))
        results = self._impl.diagnostics(dbg)
        if not isinstance(results, str) and not isinstance(results, unicode):
            raise (TypeError("string", repr(results)))
        return results
    def _dispatch(self, method, params):
        """type check inputs, call implementation, type check outputs and return"""
        args = params[0]
        if method == "Plugin.query":
            return success(self.query(args))
        elif method == "Plugin.ls":
            return success(self.ls(args))
        elif method == "Plugin.diagnostics":
            return success(self.diagnostics(args))
class Plugin_skeleton:
    """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        raise Unimplemented("Plugin.query")
    def ls(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        raise Unimplemented("Plugin.ls")
    def diagnostics(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        raise Unimplemented("Plugin.diagnostics")
class Plugin_test:
    """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
    def __init__(self):
        pass
    def query(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        result = {}
        result["query_result"] = { "plugin": "string", "name": "string", "description": "string", "vendor": "string", "copyright": "string", "version": "string", "required_api_version": "string", "features": [ "string", "string" ], "configuration": { "string": "string" }, "required_cluster_stack": [ "string", "string" ] }
        return result
    def ls(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        result = {}
        result["srs"] = [ "string", "string" ]
        return result
    def diagnostics(self, dbg):
        """Discover properties of this implementation. Every implementation  must support the query interface or it will not be recognised as  a storage plugin by xapi."""
        result = {}
        result["diagnostics"] = "string"
        return result
class Plugin_commandline():
    """Parse command-line arguments and call an implementation."""
    def __init__(self, impl):
        self.impl = impl
        self.dispatcher = Plugin_server_dispatcher(self.impl)
    def _parse_query(self):
        """Query this implementation and return its properties. This is  called by xapi to determine whether it is compatible with xapi  and to discover the supported features."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='Query this implementation and return its properties. This is  called by xapi to determine whether it is compatible with xapi  and to discover the supported features.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        return vars(parser.parse_args())
    def _parse_ls(self):
        """[ls dbg]: returns a list of attached SRs"""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='[ls dbg]: returns a list of attached SRs')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        return vars(parser.parse_args())
    def _parse_diagnostics(self):
        """Returns a printable set of backend diagnostic information. Implementations are encouraged to include any data which will  be useful to diagnose problems. Note this data should not  include personally-identifiable data as it is intended to be  automatically included in bug reports."""
        # in --json mode we don't have any other arguments
        if ('--json' in sys.argv or '-j' in sys.argv):
            jsondict = json.loads(sys.stdin.readline(),)
            jsondict['json'] = True
            return jsondict
        parser = argparse.ArgumentParser(description='Returns a printable set of backend diagnostic information. Implementations are encouraged to include any data which will  be useful to diagnose problems. Note this data should not  include personally-identifiable data as it is intended to be  automatically included in bug reports.')
        parser.add_argument('-j', '--json', action='store_const', const=True, default=False, help='Read json from stdin, print json to stdout', required=False)
        parser.add_argument('dbg', action='store', help='Debug context from the caller')
        return vars(parser.parse_args())
    def query(self):
        use_json = False
        try:
            request = self._parse_query()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.query(request)
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
    def diagnostics(self):
        use_json = False
        try:
            request = self._parse_diagnostics()
            use_json = 'json' in request and request['json']
            results = self.dispatcher.diagnostics(request)
            print json.dumps(results)
        except Exception, e:
            if use_json:
                xapi.handle_exception(e)
            else:
                traceback.print_exc()
                raise e
class plugin_server_dispatcher:
    """Demux calls to individual interface server_dispatchers"""
    def __init__(self, Plugin=None):
        self.Plugin = Plugin
    def _dispatch(self, method, params):
        try:
            logging.debug("method = %s params = %s" % (method, repr(params)))
            if method.startswith("Plugin") and self.Plugin:
                return self.Plugin._dispatch(method, params)
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
class plugin_server_test(plugin_server_dispatcher):
    """Create a server which will respond to all calls, returning arbitrary values. This is intended as a marshal/unmarshal test."""
    def __init__(self):
        plugin_server_dispatcher.__init__(self, Plugin_server_dispatcher(Plugin_test()))