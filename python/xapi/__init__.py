#!/usr/bin/env python

import sys
import traceback
import json
import argparse


def success(result):
    return {"Status": "Success", "Value": result}


def handle_exception(e, code=None, params=None):
    s = sys.exc_info()
    files = []
    lines = []
    for slot in traceback.extract_tb(s[2]):
        files.append(slot[0])
        lines.append(slot[1])
    backtrace = {
        "error": str(s[1]),
        "files": files,
        "lines": lines,
    }
    code = "SR_BACKEND_FAILURE"
    params = [str(s[1])]
    if hasattr(e, "code"):
        code = e.code
    if hasattr(e, "params"):
        params = e.params
    results = {
        "code": code,
        "params": params,
        "backtrace": backtrace,
    }
    print >>sys.stdout, json.dumps(results)
    sys.exit(1)


class XenAPIException(Exception):

    def __init__(self, code, params):
        Exception.__init__(self)
        if not isinstance(code, str) and not isinstance(code, unicode):
            raise (TypeError("string", repr(code)))
        if not isinstance(params, list):
            raise (TypeError("list", repr(params)))
        self.code = code
        self.params = params


class MissingDependency(Exception):

    def __init__(self, missing):
        self.missing = missing

    def __str__(self):
        return "There is a missing dependency: %s not found" % self.missing


class Rpc_light_failure(Exception):

    def __init__(self, name, args):
        self.name = name
        self.args = args

    def failure(self):
        # rpc-light marshals a single result differently to a list of results
        args = list(self.args)
        marshalled_args = args
        if len(args) == 1:
            marshalled_args = args[0]
        return {'Status': 'Failure',
                'ErrorDescription': [self.name, marshalled_args]}


class InternalError(Rpc_light_failure):

    def __init__(self, error):
        Rpc_light_failure.__init__(self, "Internal_error", [error])


class UnmarshalException(InternalError):

    def __init__(self, thing, ty, desc):
        InternalError.__init__(
            self,
            "UnmarshalException thing=%s ty=%s desc=%s" % (thing, ty, desc))


class TypeError(InternalError):

    def __init__(self, expected, actual):
        InternalError.__init__(
            self, "TypeError expected=%s actual=%s" % (expected, actual))


class UnknownMethod(InternalError):

    def __init__(self, name):
        InternalError.__init__(self, "Unknown method %s" % name)


def is_long(x):
    try:
        long(x)
        return True
    except ValueError:
        return False


class ListAction(argparse.Action):

    def __call__(self, parser, namespace, values, option_string=None):
        k = values[0]
        v = values[1]
        if ((hasattr(namespace, self.dest)
             and getattr(namespace, self.dest) is not None)):
            getattr(namespace, self.dest)[k] = v
        else:
            setattr(namespace, self.dest, {k: v})
