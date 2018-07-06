#!/usr/bin/env python
import argparse
import threading
import logging
import subprocess
from scapy.all import Ether, IP, sendp
from scapy.contrib.igmp import IGMP
from xcp import logger as log
import sys
import os
from xen.lowlevel.xs import xs


# global xenstore handler
g_xs_handler = xs()
VIF_CONNECTED_STATE = '4'


class XSWatcher(object):
    """Tool for watching xenstore
    """
    def __init__(self):
        self.watches = dict()

    def watch(self, path, token):
        self.watches[path] = token
        return g_xs_handler.watch(path, token)

    def unwatch(self, path, token):
        self.watches.pop(path)
        return g_xs_handler.unwatch(path, token)

    def read_watch(self):
        return g_xs_handler.read_watch()


class IGMPQueryInjector(object):
    def __init__(self, max_resp_time, vifs, vif_connected_timeout=0):
        self.max_resp_time = max_resp_time
        self.vifs = vifs
        self.vif_connected_timeout = vif_connected_timeout

    def inject_to_vif(self, vif):
        mac = get_vif_mac(vif)
        try:
            self.inject_packet(vif, mac)
            log.info('Inject IGMP query to vif:%s, mac:%s' % (vif, mac))
        except:
            log.error('Inject IGMP query to vif:%s, mac:%s failed' % (vif, mac))

    def inject_packet(self, iface, dst_mac):
        ether_part = Ether(src='00:00:00:00:00:00', dst=dst_mac)
        ip_part = IP(ttl=1, src='0.0.0.0', dst='224.0.0.1')
        igmp_part = IGMP(type=0x11)
        igmp_part.mrtime = (self.max_resp_time / 100) & 0xff
        igmp_part.igmpize(ether=ether_part, ip=ip_part)
        # Make this IGMP query packet as an unicast packet
        ether_part.dst = dst_mac
        sendp(ether_part / ip_part / igmp_part, iface=iface, verbose=False)

    def inject(self):
        if not self.vifs:
            return

        if self.vif_connected_timeout > 0:
            # should check connection state
            log.info('Inject IGMP query with connection state check')
            self._inject_with_connection_state_check()
        else:
            log.info('Inject IGMP query without connection state check')
            self._inject_without_connection_state_check()

    def check_and_inject(self, watcher):
        vif_connected_set = set()
        while watcher.watches:
            path, vif = watcher.read_watch()
            if g_xs_handler.read('', path) == VIF_CONNECTED_STATE:
                # should ensure both backend and frontend connected before injection
                if vif not in vif_connected_set:
                    vif_connected_set.add(vif)
                else:
                    vif_connected_set.remove(vif)
                    self.inject_to_vif(vif)
                watcher.unwatch(path, vif)

    def _inject_with_connection_state_check(self):
        watcher = XSWatcher()
        for vif in self.vifs:
            state_path, backend_state_path = get_vif_state_path(vif)
            # watch both frontend and backend vif state
            watcher.watch(state_path, vif)
            watcher.watch(backend_state_path, vif)

        # We have 2 options to handle the blocking function `xs.read_watch`
        # 1. Single thread with Unix alarm signal
        #    The signal handler cannot be properly invoked because when signal received the process context is in
        #    C extension but not python interpreter, so the interpreter cannot call the signal handler.
        #    So we should not apply this option.
        # 2. Multi thread with threading.event or timeout associated join
        #    Create a new thread to invoke the blocking methon `xs.read_watch`. In the main thread using a timeout
        #    associated join method for waiting. We will apply this option.
        t = threading.Thread(target=self.check_and_inject, args=(watcher,))
        t.daemon = True
        t.start()
        t.join(self.vif_connected_timeout)
        if watcher.watches:
            log.warning('Wait vif state change timeout')
            for vif in watcher.watches.itervalues():
                log.warning("Vif:%s state did not change to '%s', don't inject IGMP query to mac: %s" %
                            (vif, VIF_CONNECTED_STATE, get_vif_mac(vif)))

    def _inject_without_connection_state_check(self):
        for vif in self.vifs:
            self.inject_to_vif(vif)


def domid_vifid_of_vif(vif):
    return [int(x) for x in vif.split('vif')[1].split('.')]


def vif_frontend_path(vif):
    domid, vifid = domid_vifid_of_vif(vif)
    dompath = g_xs_handler.get_domain_path(domid)
    return os.path.join(os.path.join(dompath, 'device', 'vif', '%d' % vifid))


def vif_backend_path(vif):
    dompath = g_xs_handler.get_domain_path(0)
    domid, vifid = domid_vifid_of_vif(vif)
    return os.path.join(os.path.join(dompath, 'backend', 'vif', '%d' % domid, '%d' % vifid))


def get_vif_mac(vif):
    return g_xs_handler.read('', os.path.join(vif_frontend_path(vif), 'mac'))


def get_vif_state_path(vif):
    """return frontend and backend vif state path
    """
    return os.path.join(vif_frontend_path(vif), 'state'), os.path.join(vif_backend_path(vif), 'state')


def get_parent_bridge(bridge):
    return subprocess.check_output(['/usr/bin/ovs-vsctl', 'br-to-parent', bridge]).strip()


def network_backend_is_openvswitch():
    bridge_type = subprocess.check_output(['/opt/xensource/bin/xe-get-network-backend']).strip()
    return bridge_type == 'openvswitch'


def memodict(f):
    """ Memoization decorator for a function taking a single argument """
    class Memodict(dict):
        def __missing__(self, key):
            ret = self[key] = f(key)
            return ret
    return Memodict().__getitem__


@memodict
def igmp_snooping_is_enabled_on_bridge(bridge):
    vlan = subprocess.check_output(['/usr/bin/ovs-vsctl', 'br-to-vlan', bridge]).strip()
    if vlan != '0':
        # this br is a fake br, should get its parent
        bridge = get_parent_bridge(bridge)
    return _igmp_snooping_is_enabled_on_bridge(bridge)


@memodict
def _igmp_snooping_is_enabled_on_bridge(bridge):
    enabled = subprocess.check_output(['/usr/bin/ovs-vsctl', 'get', 'bridge', bridge, 'mcast_snooping_enable']).strip()
    return enabled == 'true'


def igmp_snooping_is_enabled_on_bridge_of_vif(vif):
    bridge = subprocess.check_output(['/usr/bin/ovs-vsctl', 'iface-to-br', vif]).strip()
    return igmp_snooping_is_enabled_on_bridge(bridge)


def inject_to_vifs(args):
    log.debug('Entry point: Inject IGMP query per pif')
    if args.no_check_snooping_toggle:
        vifs = args.vifs
    else:
        vifs = [vif for vif in args.vifs if igmp_snooping_is_enabled_on_bridge_of_vif(vif)]
    injector = IGMPQueryInjector(args.max_resp_time, vifs, args.vif_connected_timeout)
    return injector.inject()


def build_parser():
    parser = argparse.ArgumentParser(prog='igmp_query_injector.py', description='Tool for injecting IGMP query packet')
    parser.add_argument('--max-resp-time', dest='max_resp_time', required=False, metavar='max_resp_time', type=int,
                        default=100, help='max response time of IGMP query, unit is millisecond')
    parser.add_argument('--verbose', dest='verbose', required=False, action='store_true',
                        help='print verbose log')
    parser.add_argument('--wait-vif-connected', dest='vif_connected_timeout', metavar='timeout', type=int,
                        default=0, help='timeout value for waiting vif connected, unit is second')
    parser.add_argument('--no-check-snooping-toggle', dest='no_check_snooping_toggle', required=False,
                        action='store_true', help='do not need to check IGMP snooping toggle')
    parser.add_argument('vifs', metavar='vif_name', nargs='+', help='vif interface name in Dom0')
    return parser


def main():
    args = build_parser().parse_args()

    logging_lvl = logging.INFO
    if args.verbose:
        logging_lvl = logging.DEBUG

    log.logToSyslog(level=logging_lvl)

    if not network_backend_is_openvswitch():
        log.info('Network backend type is not openvswitch, no need to inject query')
        sys.exit(0)

    inject_to_vifs(args)


if __name__ == '__main__':
    main()
