#!/usr/bin/env python
import Queue
import argparse
import os
import socket
import struct
import subprocess
import threading
from abc import ABCMeta, abstractmethod
import logging
import signal
import resource
import binascii
from xcp import logger as log
import time
import sys
import re
from xen.lowlevel.xs import xs


class Cleanup(object):
    """Ensure xenstore connection closed when exit
    """
    CATCH_SIGNALS = [signal.SIGTERM, signal.SIGINT]

    def __enter__(self):
        self._set_signal_handler(self.CATCH_SIGNALS, self._close_xs_connection)

    def __exit__(self, exc_type, exc_value, traceback):
        XSUtil.close()

    def _set_signal_handler(self, signals, f):
        for s in signals:
            signal.signal(s, f)

    def _close_xs_connection(self, signum, frame):
        log.critical('Catch signal %d, close xenstore connection' % signum)
        XSUtil.close()
        sys.exit(ERRNO.SIGNAL_CATCH)


class ERRNO(object):
    SUCC = 0
    SIGNAL_CATCH = 1


class XSUtil(object):
    """Utilities for access xenstore
    """

    xs_handler = xs()

    @classmethod
    def close(cls):
        cls.xs_handler.close()

    @classmethod
    def read(cls, path):
        return cls.xs_handler.read('', path)

    @classmethod
    def watch(cls, path, token):
        return cls.xs_handler.watch(path, token)

    @classmethod
    def unwatch(cls, path, token):
        return cls.xs_handler.unwatch(path, token)

    @classmethod
    def read_watch(cls):
        return cls.xs_handler.read_watch()

    @staticmethod
    def vif_mac_path(domid, vifid):
        return '/local/domain/%d/device/vif/%d/mac' % (domid, vifid)

    @staticmethod
    def vif_state_path(domid, vifid):
        return '/local/domain/%d/device/vif/%d/state' % (domid, vifid)


class XSWatch(object):
    __metaclass__ = ABCMeta
    """Tool for watching xenstore
    """
    def __init__(self):
        self.watches = []

    def watch(self, path, token):
        self.watches.append(path)
        return XSUtil.watch(path, token)

    def unwatch(self, path, token):
        self.watches.remove(path)
        return XSUtil.unwatch(path, token)

    def process_all_watches(self):
        while self.watches:
            path, token = XSUtil.read_watch()
            if self.val_change_to_expected(path, token):
                self.unwatch(path, token)
                self.put_task(path, token)

    @abstractmethod
    def val_change_to_expected(self, path, token):
        pass

    @abstractmethod
    def put_task(self, path, token):
        pass


class XSVifStateWatch(XSWatch):
    def __init__(self, expected_state):
        super(XSVifStateWatch, self).__init__()
        self.expected_state = expected_state
        self.queue = Queue.Queue()

    def val_change_to_expected(self, path, vif):
        return XSUtil.read(vif.state_path) == self.expected_state

    def put_task(self, path, vif):
        self.queue.put(InjectTask(vif, len(self.watches) == 0))

    def get_task(self, timeout):
        return self.queue.get(timeout=timeout)


class InjectTask(object):
    def __init__(self, vif, is_last):
        self.vif = vif
        self.is_last = is_last


class Vif(object):
    def __init__(self, vif):
        self.vif_name = vif
        ids = self.vif_name.split('vif')[1].split('.')
        self._domid = int(ids[0])
        self._vifid = int(ids[1])
        self.mac = XSUtil.read(XSUtil.vif_mac_path(self._domid, self._vifid))
        self.state_path = XSUtil.vif_state_path(self._domid, self._vifid)
        self._injected = False

    def mark_injected(self):
        self._injected = True

    @property
    def injected(self):
        return self._injected


class IGMPQueryGenerator(object):
    """IGMP Query generator.
    Generate IGMP Query packet with/without vlan
    """
    def __init__(self, dst_mac, vlanid, max_resp_time):
        """
        :param dst_mac: Destination mac address of this IGMP query packet
        :param vlanid: Vlan ID of this packet. `-1` means no vlan
        :param max_resp_time: Max response time of IGMP query. Unit is 100ms
        """
        self.src_mac = '00:00:00:00:00:00'
        self.dst_mac = dst_mac
        self.vlanid = vlanid
        self.max_resp_time = max_resp_time

    def mac2binarry(self, mac):
        return binascii.unhexlify(mac.replace(':', ''))

    def create_ether_layer(self):
        if self.vlanid == -1:
            type_field = 0x0800
        else:
            type_field = 0x8100
        return struct.pack('!6s6sH', self.mac2binarry(self.dst_mac), self.mac2binarry(self.src_mac), type_field)

    def create_vlan_layer(self):
        if self.vlanid == -1:
            return struct.pack('')
        else:
            return struct.pack('!HH', 0x2000 | self.vlanid, 0x0800)

    def create_ip_layer(self):
        """
        IP Type: IPv4 (0x0800)
        Version: 4
        Total length: 32
        TTL: 1
        Source IP: 0.0.0.0
        Destination IP: 224.0.0.1 (e0:00:00:01)
        """
        hex_str = "4600002000010000010244d600000000e000000194040000"
        s = binascii.unhexlify(hex_str)
        return struct.pack('!%ds' % len(s), s)

    def create_igmp_layer(self):
        """
        IGMP Version: 2
        IGMP Type: IGMP Query (0x11)
        Max Resp Time: self.max_resp_time
        Multicast Address: 0.0.0.0
        """
        msg_type = 0x11
        # MAX_RESP_TIME filed unit is 100ms
        max_resp_time = self.max_resp_time / 100
        group_address = socket.inet_aton('0.0.0.0')
        chksum = self._calc_igmp_checksum(struct.pack('!BBH4s', msg_type, max_resp_time, 0x0000, group_address))
        return struct.pack('!BBH4s', msg_type, max_resp_time, chksum, group_address)

    def _calc_igmp_checksum(self, packet):
        def carry_around_add(a, b):
            c = a + b
            return (c & 0xffff) + (c >> 16)

        s = 0
        msg = struct.unpack('%dB' % len(packet), packet)
        for i in range(0, len(msg), 2):
            w = (msg[i] << 8) + msg[i+1]
            s = carry_around_add(s, w)
        return ~s & 0xffff

    def generate(self):
        ether_layer = self.create_ether_layer()
        vlan_layer = self.create_vlan_layer()
        ip_layer = self.create_ip_layer()
        igmp_layer = self.create_igmp_layer()
        packet = ether_layer + vlan_layer + ip_layer + igmp_layer
        return packet


class IGMPQueryInjector(object):
    __metaclass__ = ABCMeta

    def __init__(self, max_resp_time):
        self.max_resp_time = max_resp_time

    def inject_to_vif(self, vif, mac):
        log.info('Inject IGMP query to vif:%s, mac:%s' % (vif, mac))
        packet = IGMPQueryGenerator(mac, -1, self.max_resp_time).generate()
        self.inject_query_packet(vif, packet)

    def inject_to_vifs(self, vifs):
        for vif in vifs:
            _vif = Vif(vif)
            self.inject_to_vif(_vif.vif_name, _vif.mac)

    def inject_query_packet(self, interface, packet):
        s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, 0)
        s.bind((interface, 0))
        s.send(packet)
        s.close()

    @abstractmethod
    def inject(self):
        pass


class VifsInjector(IGMPQueryInjector):
    def __init__(self, max_resp_time, vifs, vif_connected_timeout=0):
        super(VifsInjector, self).__init__(max_resp_time)
        self.vifs = vifs
        self.vif_connected_timeout = vif_connected_timeout

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

    def _inject_with_connection_state_check(self):
        vifs = [Vif(vif) for vif in self.vifs]
        expected_vif_state = '4'
        watch = XSVifStateWatch(expected_vif_state)
        for vif in vifs:
            watch.watch(vif.state_path, vif)

        t = threading.Thread(target=watch.process_all_watches)
        t.daemon = True
        t.start()
        remain_time = self.vif_connected_timeout

        while remain_time > 0:
            start_time = time.time()
            try:
                task = watch.get_task(remain_time)
                vif = task.vif
                self.inject_to_vif(vif.vif_name, vif.mac)
                vif.mark_injected()
                if task.is_last:
                    return
            except Queue.Empty as e:
                # timeout
                break

            remain_time = remain_time - (time.time() - start_time)

        for vif in vifs:
            if not vif.injected:
                log.warning("Value of '%s' does not change to '%s' in %d seconds." %
                            (vif.state_path, expected_vif_state, self.vif_connected_timeout))
                log.warning("Don't inject IGMP query to vif: %s, mac: %s" % (vif.vif_name, vif.mac))

    def _inject_without_connection_state_check(self):
        self.inject_to_vifs(self.vifs)


class BridgeInjector(IGMPQueryInjector):
    RE_VIF = re.compile(r'^vif\d+\.\d+$')

    def __init__(self, max_resp_time, bridges):
        super(BridgeInjector, self).__init__(max_resp_time)
        self.bridges = bridges

    def get_vifs_on_bridge(self, bridge):
        ret = []
        outs = subprocess.check_output(['ovs-vsctl', 'list-ports', bridge]).strip()
        for line in outs.split('\n'):
            if self.RE_VIF.match(line):
                ret.append(line)

        return ret

    def inject(self):
        for bridge in self.bridges:
            log.info('Inject IGMP query to bridge:%s' % bridge)
            vifs = self.get_vifs_on_bridge(bridge)
            self.inject_to_vifs(vifs)


def inject_to_vifs(args):
    log.debug('Entry point: Inject IGMP query per pif')
    vifs = []
    for vif in args.vifs:
        if not igmp_snooping_is_enabled_on_bridge_of_vif(vif):
            log.info("IGMP snooping is disabled on bridge of interface %s, won't inject query" % vif)

        vifs.append(vif)
    injector = VifsInjector(args.max_resp_time, vifs, args.vif_connected_timeout)
    return injector.inject()


def inject_to_vifs_on_bridges(args):
    log.debug('Entry point: Inject IGMP query per bridge')
    bridges = []
    for bridge in args.bridges:
        if not igmp_snooping_is_enabled_on_bridge(bridge):
            log.info("IGMP snooping is disabled on bridge %s, won't inject query" % bridge)
        bridges.append(bridge)
    injector = BridgeInjector(args.max_resp_time, bridges)
    return injector.inject()


def build_parser():
    parser = argparse.ArgumentParser(prog='igmp_query_injector.py', description=
                                     'Tool for injecting IGMP query packet')
    parser.add_argument('--detach', dest='detach', required=False, action='store_true',
                        help='execute this tool as a daemon')
    parser.add_argument('--max-resp-time', dest='max_resp_time', required=False, metavar='max_resp_time', type=int,
                        default=100, help='max response time of IGMP query, unit is millisecond')
    parser.add_argument('--verbose', dest='verbose', required=False, action='store_true',
                        help='print verbose log')

    subparsers = parser.add_subparsers()
    to_vif_parser = subparsers.add_parser('vif', help='inject query to vifs',
                                          description='Inject query to vifs')
    to_vif_parser.set_defaults(func=inject_to_vifs)
    to_vif_parser.add_argument('vifs', metavar='vif_name', nargs='+', help='vif interface name in Dom0')
    to_vif_parser.add_argument('--wait-vif-connected', dest='vif_connected_timeout', metavar='timeout', type=int,
                               default=0, help='timeout value for waiting vif connected, unit is second')

    to_bridge_parser = subparsers.add_parser('bridge', help='inject query to vifs on the bridge',
                                             description='Inject query to vifs on the bridge')
    to_bridge_parser.set_defaults(func=inject_to_vifs_on_bridges)
    to_bridge_parser.add_argument('bridges', metavar='bridge_name', nargs='+', help='bridge name of OVS')

    return parser


def _detach():
    try:
        pid = os.fork()
        if pid > 0:
            sys.exit(0)
        else:
            maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
            if maxfd == resource.RLIM_INFINITY:
                maxfd = 1024

            # Iterate through and close all file descriptors.
            for fd in range(0, maxfd):
                try:
                    os.close(fd)
                except OSError:
                    pass
    except OSError as e:
        sys.stderr.write("fork failed: %d (%s)\n" % (e.errno, e.strerror))
        sys.exit(1)


def network_backend_is_openvswitch():
    bridge_type = subprocess.check_output(['/opt/xensource/bin/xe-get-network-backend']).strip()
    return bridge_type == 'openvswitch'


def igmp_snooping_is_enabled_on_bridge(bridge):
    enabled = subprocess.check_output(['/usr/bin/ovs-vsctl', 'get', 'bridge', bridge, 'mcast_snooping_enable']).strip()
    return enabled == 'true'


def igmp_snooping_is_enabled_on_bridge_of_vif(vif):
    bridge = subprocess.check_output(['/usr/bin/ovs-vsctl', 'iface-to-br', vif]).strip()
    return igmp_snooping_is_enabled_on_bridge(bridge)


def main():
    parser = build_parser()
    args = parser.parse_args()

    logging_lvl = logging.INFO
    if args.verbose:
        logging_lvl = logging.DEBUG

    log.logToSyslog(level=logging_lvl)

    if args.detach:
        _detach()

    if not network_backend_is_openvswitch():
        log.info('Network backend type is not openvswitch, no need to inject query')
        return ERRNO.SUCC

    # Ensure xenstore connection closed when exception/signal catches
    with Cleanup():
        args.func(args)


if __name__ == '__main__':
    main()
