#!/usr/bin/env python
"""
Script which monitors the domains running on a host, looks for
paused domains which don't correspond to VMs which are running here
or are about to run here, logs them and optionally destroys them.
"""
# DONE: PYTYPE_PASSES, MYPY_PASSES
# TODO: NEEDS_UNIT_TESTS, PYLINT_WARNINGS_IN_BAD_SHAPE, CODE_SMELLS, ADD_DOC_STRINGS
import sys
import time
from subprocess import PIPE, Popen

import inventory  # type: ignore[import-untyped] # pylint: disable=import-error
import XenAPI  # type: ignore[import-untyped]

# pylint:disable=import-outside-toplevel

if sys.version_info >= (3,):
    from typing import Dict, List, Tuple
    # Using simple type aliases here: mypy, pytype and pyright understand them,
    # and the code can still be parsed by Python2 (and ignored)
    DomID = str
    UUID = str
    DomIdUUIDTuple = Tuple[DomID, UUID]

# Return a list of (domid, uuid) tuples, one per paused domain on this host
def list_paused_domains():  # type:() -> List[DomIdUUIDTuple]
    results = []   # type: List[DomIdUUIDTuple]
    all_domains = Popen(
        ["@OPTDIR@/bin/list_domains"],
        stdout=PIPE,
        universal_newlines=True
    ).communicate()[0]
    lines = all_domains.split("\n")
    for domain in lines[1:]:
        bits = domain.split()
        if bits != []:
            domid = bits[0]
            uuid = bits[2]
            state = bits[4]
            if 'P' in state:
                results.append( (domid, uuid) )
    return results

# Given localhost's uuid and a (domid, uuid) tuple, return True if the domain
# be somewhere else i.e. we think it may have leaked here
def should_domain_be_somewhere_else(localhost_uuid, domain):
    (domid, uuid) = domain
    try:
        x = XenAPI.xapi_local()  # pytype: disable=name-error
        assert x.xenapi
        x.xenapi.login_with_password("root", "", "1.0", "xen-api-scripts-monitor-unwanted-domains.py")
        try:
            try:
                assert x.xenapi.VM
                vm = x.xenapi.VM.get_by_uuid(uuid)
                resident_on = x.xenapi.VM.get_resident_on(vm)
                current_operations = x.xenapi.VM.get_current_operations(vm)
                result = current_operations == {} and resident_on != localhost_uuid
                if result:
                    print("domid %s uuid %s: is not being operated on and is not resident here" % (domid, uuid))
                    return result
            except XenAPI.Failure as e:  # pytype: disable=name-error # XenAPI is not typed yet
                if e.details[0] == "UUID_INVALID":
                    # VM is totally bogus
                    print("domid %s uuid %s: is not in the xapi database" % (domid, uuid))
                    return True
                # fail safe for now
                return False
        finally:
            x.xenapi.logout()
    except:
        return False

# Destroy the given domain
def destroy_domain(domain):
    # pytype forgets that we import subprocess/Popen on the toplevel, allow it to work:
    from subprocess import PIPE, Popen   # pylint: disable=reimported,redefined-outer-name

    (domid, uuid) = domain
    print("destroying domid %s uuid %s" % (domid, uuid))
    Popen(["@OPTDIR@/debug/destroy_domain", "-domid", domid], stdout=PIPE).communicate()[0]

# Keep track of when a domain first looked like it should be here
domain_first_noticed = {}  # type: Dict[DomIdUUIDTuple, float]

# Number of seconds after which we conclude that a domain really shouldn't be here
THRESHOLD = 60

if __name__ == "__main__":
    localhost_uuid = inventory.get_localhost_uuid ()
    while True:
        time.sleep(1)
        paused = list_paused_domains ()

        # The reason was that this loop needs to populate domain_first_noticed,
        # (it is initially an empty dict, so the order in the loop is better like this:)

        # populate domain_first_noticed and destroy domains in bad state over threshold:
        for d in list_paused_domains():
            if should_domain_be_somewhere_else(localhost_uuid, d):
                if d not in domain_first_noticed:
                    domain_first_noticed[d] = time.time()
                noticed_for = time.time() - domain_first_noticed[d]
                if noticed_for > THRESHOLD:
                    print("domid %s uuid %s: has been in bad state for over threshold" % d)
                    if "-destroy" in sys.argv:
                        destroy_domain(d)

        # Then, garbage-collect domain_first_noticed:
        # GC the domain_first_noticed map

        # On pylint disable: This needs to iterate of the keys()->list[DomIdUUIDTuple],
        # not the item(s), which is time(): domain_first_noticed[d] = time.time()
        for dom_id_uuid_tuple in domain_first_noticed:  # pylint: disable=consider-using-dict-items
            if dom_id_uuid_tuple not in paused:
                print("domid %s uuid %s: looks ok now, forgetting about it" % dom_id_uuid_tuple)
                del domain_first_noticed[dom_id_uuid_tuple]
