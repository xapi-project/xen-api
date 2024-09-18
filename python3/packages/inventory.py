#!/usr/bin/env python3

"""
inventory.py

This module defines functions to read and parse constants from the xensource-inventory file.
"""
import sys

INVENTORY = "@INVENTORY@"
INSTALLATION_UUID = "INSTALLATION_UUID"


def read_kvpairs(filename):
    """Read in a file of key-value pairs in the format used by the inventory file"""
    all_entries = {}
    with open(filename, 'r', encoding='utf-8') as f:
        for line in f:
            equals = line.index("=")
            key = line[:equals]
            value = line[equals+1:].strip().strip("'")
            all_entries[key] = value
    return all_entries


def parse():
    """Return the contents of the xensource inventory file as a dictionary"""
    try:
        return read_kvpairs(INVENTORY)
    except FileNotFoundError as e:
        print("Error: File '{}' not found. {}".format(INVENTORY, e), file=sys.stderr)
        return {}


def get_localhost_uuid():
    """Return the UUID of the local host"""
    return parse()[INSTALLATION_UUID]
