# Simple functions to read the constants from the xensource-inventory file

INVENTORY="@INVENTORY@"
INSTALLATION_UUID="INSTALLATION_UUID"


def read_kvpairs(filename):
   """Read in a file of key-value pairs in the format used by the inventory file"""
   f = open(filename)
   all_entries = {}
   try:
        for line in f.readlines():
            equals = line.index("=")    
            key = line[0:equals]
            value = line[equals+1:].strip().strip("'")
            all_entries[key] = value
   finally:
        f.close()
        return all_entries


def parse():
    """Return the contents of the xensource inventory file as a dictionary"""
    try:
        return read_kvpairs(INVENTORY)  
    except:
        return {}


def get_localhost_uuid():
    """Return the UUID of the local host"""
    return parse()[INSTALLATION_UUID]
