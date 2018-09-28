#!/usr/bin/env python
import XenAPI
import argparse

def build_parser():
    parser = argparse.ArgumentParser(prog='send_message.py', description='Call xapi to send message')
    parser.add_argument('--name', dest='name', required=True, metavar='msg_name', type=str,
                        help='message name')
    parser.add_argument('--priority', dest='priority', required=True, metavar='message_priority', type=int,
                        help='message priority')
    parser.add_argument('--class', dest='cls', required=True, metavar='message_class', type=str,
                        help='message class')
    parser.add_argument('--uuid', dest='uuid', required=True, metavar='obj_uuid', type=str,
                        help='uuid of the object this message is associated with')
    parser.add_argument('--body', dest='body', required=True, metavar='message_body', type=str,
                        help='message body')
    return parser


def main():
    args = build_parser().parse_args()
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password('root', '', '', 'send_message.py')

    try:
        session.xenapi.message.create(args.name, args.priority, args.cls, args.uuid, args.body)
    finally:
        session.xenapi.logout()


if __name__ == '__main__':
    main()
