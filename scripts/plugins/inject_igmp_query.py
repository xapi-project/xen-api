#!/usr/bin/python
import argparse
import socket


def parse_mac_address(mac):
    ret = []
    for x in mac.split(':'):
        ret.append(int(x, 16))

    return ret


def create_ether_layer(dst_mac, src_mac, eth_type):
    ret = []
    for mac in (dst_mac, src_mac):
        ret.extend(parse_mac_address(mac))

    type_field = None
    if eth_type == 'ip':
        type_field = [0x08, 0x00]
    elif eth_type == 'vlan':
        type_field = [0x81, 0x00]

    ret.extend(type_field)
    return ret


def create_vlan_layer(vlanid):
    return [0x20 | (vlanid >> 8), vlanid & 0xff, 0x08, 0x00]


def create_ip_layer():
    """
    IP Type: IPv4 (0x0800)
    Version: 4
    Total length: 32
    TTL: 1
    Source IP: 0.0.0.0
    Destination IP: 224.0.0.1 (e0:00:00:01)
    """

    return [
        0x46, 0x00, 0x00, 0x20, 0x00, 0x01,
        0x00, 0x00, 0x01, 0x02, 0x44, 0xd6,
        0x00, 0x00, 0x00, 0x00, 0xe0, 0x00,
        0x00, 0x01, 0x94, 0x04, 0x00, 0x00,
    ]


def create_igmp_layer():
    """
    IGMP Type: IGMP Query (0x11)
    Max Resp Time: 0.1 second (0x01)
    """

    return [
        0x11, 0x01, 0xee, 0xfe,
        0x00, 0x00, 0x00, 0x00,
    ]


def create_igmp_query(dst_mac, vlanid):
    vlan_layer = []

    if vlanid == 0:
        eth_type = 'ip'
    else:
        eth_type = 'vlan'
        vlan_layer = create_vlan_layer(vlanid)

    ether_layer = create_ether_layer(dst_mac, '00:00:00:00:00:00', eth_type)
    ip_layer = create_ip_layer()
    igmp_layer = create_igmp_layer()

    packet = ether_layer + vlan_layer + ip_layer + igmp_layer
    return "".join(map(chr, packet))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--interface', dest='interface', required=True, help='Vif interface name')
    parser.add_argument('--dst_mac', dest='dst_mac', required=True, help='Destination mac address')
    parser.add_argument('--vlanids', dest='vlanids', nargs='+', type=int, required=True, help='Vlan ID')
    args = parser.parse_args()

    s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, 0)
    s.bind((args.interface, 0))
    for vlan in args.vlanids:
        packet = create_igmp_query(args.dst_mac, vlan)
        s.send(packet)
    s.close()




