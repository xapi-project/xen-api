#!/usr/bin/python

"""
Returns a list of extents with their block statuses for an NBD export.

This program uses new NBD capabilities introduced in QEMU 2.12.

It uses the BLOCK_STATUS NBD extension, which relies on the structured replies
functionality. These are documented in the NBD protocol docs:
https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md
"""

import argparse
import json
import logging
import logging.handlers

from python_nbd_client import PythonNbdClient, assert_protocol
import python_nbd_client


LOGGER = logging.getLogger("get_nbd_extents")
LOGGER.setLevel(logging.DEBUG)
# The log level of python_nbd_client is not set, therefore it will default to
# that of the root logger, which is WARNING by default.

# Request length is a 32-bit int.
# It looks like for qemu 2.12, the length parameter of a NBD_CMD_BLOCK_STATUS
# request is not limited by the maximum block size supported by the server (as
# defined by NBD_INFO_BLOCK_SIZE), only by the size of a 32-bit int.
MAX_REQUEST_LEN = 2 ** 32 - 1

# Make the NBD_CMD_BLOCK_STATUS request aligned to 512 bytes, just in case. But
# it looks like this is not required for qemu 2.12.
MAX_REQUEST_LEN = MAX_REQUEST_LEN - (MAX_REQUEST_LEN % 512)


def _get_extents(path, exportname, offset, length):
    with PythonNbdClient(address=path,
                         exportname=exportname,
                         unix=True,
                         use_tls=False,
                         connect=False) as client:

        client.negotiate_structured_reply()

        # Select our metadata context. This context is documented at
        # https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md#baseallocation-metadata-context
        context = 'base:allocation'
        selected_contexts = client.set_meta_contexts(exportname, [context])
        assert_protocol(len(selected_contexts) == 1)
        (meta_context_id, meta_context_name) = selected_contexts[0]
        assert_protocol(meta_context_name == context)

        client.connect(exportname)

        size = client.get_size()
        LOGGER.debug(
            'Connected to NBD export %s served at path %s of size %d bytes',
            exportname, path, size)

        if (offset < 0) or (length <= 0) or ((offset + length) > size):
            raise ValueError("Offset={} and length={} out of bounds: "
                             "export has size {}".format(offset, length, size))
        end = offset + length
        while offset < end:
            request_len = min(MAX_REQUEST_LEN, end - offset)
            replies = client.query_block_status(offset, request_len)

            # Process the returned structured reply chunks
            # "For a successful return, the server MUST use a structured reply,
            # containing exactly one chunk of type NBD_REPLY_TYPE_BLOCK_STATUS
            # per selected context id"
            assert_protocol(len(replies) == 1)
            reply = replies[0]

            # First make sure it's a block status reply
            if python_nbd_client.is_error_chunk(
                    reply_type=reply['reply_type']):
                raise Exception('Received error: {}'.format(reply))
            if reply['reply_type'] != \
                    python_nbd_client.NBD_REPLY_TYPE_BLOCK_STATUS:
                raise Exception('Unexpected reply: {}'.format(reply))

            # Then process the returned block status info
            assert_protocol(reply['context_id'] == meta_context_id)
            # Note: There might be consecutive descriptors with the same status
            # value.
            descriptors = reply['descriptors']
            for i, descriptor in enumerate(descriptors):
                (extent_length, flags) = descriptor
                if i == extent_length:
                    # The first N-1 extents must be smaller than the requested
                    # length, but the last extent can exceed the requested
                    # length
                    extent_length = min(extent_length, end - offset)
                yield {'length': extent_length, 'flags': flags}
                offset += extent_length
                assert_protocol(offset <= end)


def _main():
    # Configure the root logger to log into syslog
    # (Specifically, into /var/log/user.log)
    syslog_handler = logging.handlers.SysLogHandler(
        address='/dev/log',
        facility=logging.handlers.SysLogHandler.LOG_USER)
    # Ensure the program name is included in the log messages:
    formatter = logging.Formatter('%(name)s: [%(levelname)s] %(message)s')
    syslog_handler.setFormatter(formatter)
    logging.getLogger().addHandler(syslog_handler)

    try:
        parser = argparse.ArgumentParser(
            description="Return a list of extents with their block statuses. "
                        "The returned extents are consecutive, non-"
                        "overlapping, in the correct order starting from the "
                        "specified offset, and exactly cover the requested "
                        "area. There might be consecutive extents with the "
                        "same status flags.")
        parser.add_argument(
            '--path',
            required=True,
            help="The path of the Unix domain socket of the NBD server")
        parser.add_argument(
            '--exportname',
            required=True,
            help="The export name of the device to connect to")
        parser.add_argument(
            '--offset',
            required=True,
            type=int,
            help="The returned list of extents will be computed "
                 "starting from this offset")
        parser.add_argument(
            '--length',
            required=True,
            type=int,
            help="The returned list of extents will be computed "
                 "for an area of this length starting at the given offset")

        args = parser.parse_args()
        LOGGER.debug('Called with args %s', args)

        extents = list(
            _get_extents(
                path=args.path,
                exportname=args.exportname,
                offset=args.offset,
                length=args.length))
        print json.dumps(extents)
    except Exception as exc:
        LOGGER.exception(exc)
        raise


if __name__ == '__main__':
    _main()
