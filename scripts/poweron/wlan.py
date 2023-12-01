#!/usr/bin/env python3

# Script which shows how to use the XenAPI to find a particular Host's management interface
# and send it a wake-on-LAN packet. Used for the power-on functionality as well.

import socket
import syslog
import time

import inventory
import xcp.cmd as cmd
import XenAPIPlugin


def find_interface_broadcast_ip(interface):
    """Return the broadcast IP address of the supplied local interface"""
    (rc, stdout, stderr) = cmd.runCmd(
        ["ip", "address", "show", "dev", interface], with_stdout=True, with_stderr=True
    )
    if rc != 0:
        raise Exception(
            "Failed to find IP address of local network interface %s: %s"
            % (interface, stderr)
        )
    words = stdout.split()
    try:
        inet_idx = words.index("inet")
        brd_idx = words.index("brd", inet_idx)
        return words[brd_idx + 1]
    except:
        raise Exception(
            "Failed to parse the output of the 'ip' command; failed to find the IP address of interface %s: %s"
            % (interface, stderr)
        )


def find_host_mgmt_pif(session, host_uuid):
    """Return the PIF object representing the management interface on a Host"""
    # First find the object representing the Host
    host = session.xenapi.host.get_by_uuid(host_uuid)
    # Second enumerate all the physical interfaces (PIFs) on that Host
    pifs = session.xenapi.host.get_PIFs(host)
    # Find the PIF which has the management flag set: this is the interface we use to perform
    # API/management operations
    mgmt = None
    for pif in pifs:
        if session.xenapi.PIF.get_management(pif):
            mgmt = pif
            break
    if mgmt is None:
        raise Exception(
            "Failed to find a management interface (PIF) for host uuid %s" % host_uuid
        )
    return mgmt


def get_physical_pif(session, pif_ref):
    """Return the PIF object underlying an interface"""
    # When the management interface is on a VLAN, the actual PIF needs to be found
    vlan = session.xenapi.PIF.get_VLAN_master_of(pif_ref)
    if vlan != "OpaqueRef:NULL":
        pif_ref = session.xenapi.VLAN.get_tagged_PIF(vlan)

    return pif_ref


def wake_on_lan(session, host, remote_host_uuid):
    # Find this Host's management interface:
    this_pif = find_host_mgmt_pif(session, inventory.get_localhost_uuid())
    # Find the name of the bridge to which it is connected:
    this_network = session.xenapi.PIF.get_network(this_pif)
    this_bridge = session.xenapi.network.get_bridge(this_network)
    # The management IP address is on the dom0 bridge (not the backend device)
    broadcast_addr = find_interface_broadcast_ip(this_bridge)

    # Find the remote Host's management interface:
    mgmt_pif = find_host_mgmt_pif(session, remote_host_uuid)
    # Find the actual physical pif
    remote_pif = get_physical_pif(session, mgmt_pif)
    # Find the MAC address of the management interface:
    mac = session.xenapi.PIF.get_MAC(remote_pif)

    """Attempt to wake up a machine by sending Wake-On-Lan packets encapsulated within UDP datagrams
    sent to the broadcast_addr."""
    # A Wake-On-LAN packet contains FF:FF:FF:FF:FF:FF followed by 16 repetitions of the target MAC address
    bin_payload = bytes.fromhex("F" * 12 + mac.replace(":", "") * 16)

    finished = False
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
        addr = (broadcast_addr, 9)  # Port 0, 7 or 9
        s.connect(addr)

        # Send WoL packets every 5 seconds for 5 minutes, waiting to see if the Host_metrics.live flag is est
        attempts = 0
        metrics = None
        while not finished and (attempts < 60):
            attempts = attempts + 1
            syslog.syslog(
                "Attempt %d sending WoL packet for MAC %s to %s"
                % (attempts, mac, broadcast_addr)
            )
            s.send(bin_payload)
            time.sleep(5)
            metrics = session.xenapi.host.get_metrics(host)
            try:
                finished = session.xenapi.host_metrics.get_live(metrics)
            except:
                pass

    return str(finished)


def main(session, args):
    remote_host_uuid = args["remote_host_uuid"]

    # Find the remote Host
    remote_host = session.xenapi.host.get_by_uuid(remote_host_uuid)

    return wake_on_lan(session, remote_host, remote_host_uuid)


if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})
