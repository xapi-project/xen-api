#!/usr/bin/python
"""CLI Front end to call commands on guest VMs"""
from __future__ import print_function

import sys
import argparse
import guestcommandlib

def parse_arguments(args):
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description='Send a message to a guest and receive a response.')
    parser.add_argument(
        '--tty',
        help="specify the tty of the console of the guest to connect to",
        required=True)
    parser.add_argument(
        '--baud',
        help="specify the baud rate of the tty",
        type=int,
        default=9600)
    parser.add_argument(
        'remote_command',
        help="command to send to the selected guest")
    return parser.parse_args(args)


def call_command(namespace):
    """Call a command with a parse_arguments provided namespace"""
    with guestcommandlib.Tty(namespace.tty, namespace.baud) as serialport:
        commands = guestcommandlib.GuestCommands(serialport)
        for output in commands.execute(namespace.remote_command):
            sys.stdout.write(output)


if __name__ == "__main__":
    call_command(parse_arguments(sys.argv[1:]))
