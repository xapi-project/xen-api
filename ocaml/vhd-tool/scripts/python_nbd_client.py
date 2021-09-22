#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright (C) 2013 Nodalink, SARL.
#
# Simple nbd client used to connect to qemu-nbd
#
# author: Benoît Canet <benoit.canet@irqsave.net>
#
# This work is open source software, licensed under the terms of the
# BSD license as described in the LICENSE file in the top-level directory.
#

# Original file from
# https://github.com/cloudius-systems/osv/blob/master/scripts/nbd_client.py ,
# added support for (non-fixed) newstyle negotation,
# then @thomasmck added support for fixed-newstyle negotiation and TLS

"""
A pure-Python NBD client.

This client implement the NBD protocol, and supports both the oldstyle and
newstyle negotiations:
https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md
Additionally, it supports the BLOCK_STATUS extension:
for the extension docs, see the same file in the extension-blockstatus branch.
"""

import socket
import struct
import ssl
import logging

LOGGER = logging.getLogger('python_nbd_client')

# Request types
NBD_CMD_READ = 0
NBD_CMD_WRITE = 1
# a disconnect request
NBD_CMD_DISC = 2
NBD_CMD_FLUSH = 3
NBD_CMD_BLOCK_STATUS = 7

# Transmission flags
NBD_FLAG_HAS_FLAGS = (1 << 0)
NBD_FLAG_SEND_FLUSH = (1 << 2)

# Client flags
NBD_FLAG_C_FIXED_NEWSTYLE = (1 << 0)

# Option types
NBD_OPT_EXPORT_NAME = 1
NBD_OPT_ABORT = 2
NBD_OPT_STARTTLS = 5
NBD_OPT_INFO = 6
NBD_OPT_STRUCTURED_REPLY = 8
NBD_OPT_LIST_META_CONTEXT = 9
NBD_OPT_SET_META_CONTEXT = 10

# Option reply types
NBD_REP_ERROR_BIT = (1 << 31)
NBD_REP_ACK = 1
NBD_REP_INFO = 3
NBD_REP_META_CONTEXT = 4

OPTION_REPLY_MAGIC = 0x3e889045565a9

NBD_REQUEST_MAGIC = 0x25609513
NBD_SIMPLE_REPLY_MAGIC = 0x67446698
NBD_STRUCTURED_REPLY_MAGIC = 0x668e33ef

# Structured reply types
NBD_REPLY_TYPE_NONE = 0
NBD_REPLY_TYPE_OFFSET_DATA = 1
NBD_REPLY_TYPE_OFFSET_HOLE = 2
NBD_REPLY_TYPE_BLOCK_STATUS = 5
NBD_REPLY_TYPE_ERROR_BIT = (1 << 15)
NBD_REPLY_TYPE_ERROR = (1 << 15 + 1)
NBD_REPLY_TYPE_ERROR_OFFSET = (1 << 15 + 2)

# Structured reply flags
NBD_REPLY_FLAG_DONE = (1 << 0)

# NBD_INFO information types
NBD_INFO_EXPORT = 0
NBD_INFO_NAME = 1
NBD_INFO_DESCRIPTION = 2
NBD_INFO_BLOCK_SIZE = 3


class NBDEOFError(EOFError):
    """
    An end of file error happened while reading from the socket, because it has
    been closed.
    """
    pass


class NBDTransmissionError(Exception):
    """
    The NBD server returned a non-zero error value in its response to a
    request.

    :attribute error_code: The error code returned by the server.
    """
    def __init__(self, error_code):
        super(NBDTransmissionError, self).__init__(
            "Server returned error during transmission: {}".format(error_code))
        self.error_code = error_code


class NBDOptionError(Exception):
    """
    The NBD server replied with an error to the option sent by the client.

    :attribute reply: The error reply sent by the server.
    """
    def __init__(self, reply):
        error_code = reply - NBD_REP_ERROR_BIT
        super(NBDOptionError, self).__init__(
            "Server returned error during option haggling: "
            "reply type={}; error code={}".format(reply, error_code))
        self.reply = reply


class NBDUnexpectedOptionResponseError(Exception):
    """
    The NBD server sent a response to an option different from the most recent
    one that the client is expecting a response to.

    :attribute expected: The option that was last sent by the client, to which
                         it is expecting a response.
    :attribute received: The server's response is a reply to this option.
    """
    def __init__(self, expected, received):
        super(NBDUnexpectedOptionResponseError, self).__init__(
            "Received response to unexpected option {}; "
            "was expecting a response to option {}"
            .format(received, expected))
        self.expected = expected
        self.received = received


class NBDUnexpectedStructuredReplyType(Exception):
    """
    The NBD server sent a structured reply chunk with an unexpected type that
    is not known by this client.

    :attribute type: The type of the structured chunk message.
    """
    def __init__(self, reply_type):
        super(NBDUnexpectedStructuredReplyType, self).__init__(
            "Received a structured reply chunk message "
            "with an unexpected type: {}".format(reply_type))
        self.reply_type = reply_type


class NBDUnexpectedReplyHandleError(Exception):
    """
    The NBD server sent a reply to a request different from the most recent one
    that the client is expecting a response to.

    :attribute expected: The handle of the most recent request that the client
                         is expecting a reply to.
    :attribute received: The server's reply contained this handle.
    """
    def __init__(self, expected, received):
        super(NBDUnexpectedReplyHandleError, self).__init__(
            "Received reply with unexpected handle {}; "
            "was expecting a response to the request with "
            "handle {}"
            .format(received, expected))
        self.expected = expected
        self.received = received


class NBDProtocolError(Exception):
    """
    The NBD server sent an invalid response that is not allowed by the NBD
    protocol.
    """
    pass


def assert_protocol(assertion):
    """Raise an NBDProtocolError if the given condition is false."""
    if assertion is False:
        raise NBDProtocolError


def _check_alignment(name, value):
    if not value % 512:
        return
    raise ValueError("%s=%i is not a multiple of 512" % (name, value))


def _is_final_structured_reply_chunk(flags):
    return flags & NBD_REPLY_FLAG_DONE == NBD_REPLY_FLAG_DONE


def is_error_chunk(reply_type):
    """
    Returns true if the structured reply chunk with the given type is an error
    chunk.
    """
    return reply_type & NBD_REPLY_TYPE_ERROR_BIT != 0


def _parse_block_status_descriptors(data):
    while data:
        (length, status_flags) = struct.unpack(">LL", data[:8])
        yield (length, status_flags)
        data = data[8:]


class PythonNbdClient(object):
    """
    A pure-Python NBD client. Supports both the fixed-newstyle and the oldstyle
    negotiation, and also has support for upgrading the connection to TLS
    during fixed-newstyle negotiation, structured replies, and the BLOCK_STATUS
    extension.
    """

    def __init__(self,
                 address,
                 exportname="",
                 port=10809,
                 timeout=60,
                 subject=None,
                 cert=None,
                 use_tls=True,
                 new_style_handshake=True,
                 unix=False,
                 connect=True):
        LOGGER.info("Creating connection to address '%s' and port '%s'",
                    address, port)
        self._flushed = True
        self._closed = True
        self._handle = 0
        self._last_sent_option = None
        self._structured_reply = False
        self._transmission_phase = False
        if unix:
            self._s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        else:
            self._s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if not unix:
            address = (address, int(port))
        self._s.settimeout(timeout)
        self._s.connect(address)
        self._closed = False
        if new_style_handshake:
            self._fixed_new_style_handshake(
                cert=cert,
                subject=subject,
                use_tls=use_tls)
            if connect:
                self.connect(exportname=exportname)
        else:
            self._old_style_handshake()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def close(self):
        """
        Sends a flush request to the server if necessary and the server
        supports it, followed by a disconnect request.
        """
        if self._transmission_phase and (not self._flushed):
            self.flush()
        if not self._closed:
            self._disconnect()
            self._closed = True

    def _recvall(self, length):
        data = bytearray(length)
        view = memoryview(data)
        bytes_left = length
        while bytes_left:
            received = self._s.recv_into(view, bytes_left)
            # If recv reads 0 bytes, that means the peer has properly
            # shut down the TCP session (end-of-file error):
            if not received:
                raise NBDEOFError
            view = view[received:]
            bytes_left -= received
        return data

    # Handshake phase

    #  Newstyle handshake

    def _send_option(self, option, data=b''):
        LOGGER.debug("NBD sending option header")
        data_length = len(data)
        LOGGER.debug("option='%d' data_length='%d'", option, data_length)
        self._s.sendall(b'IHAVEOPT')
        header = struct.pack(">LL", option, data_length)
        self._s.sendall(header + data)
        self._last_sent_option = option

    def _parse_option_reply(self):
        LOGGER.debug("NBD parsing option reply")
        reply = self._recvall(8 + 4 + 4 + 4)
        (magic, option, reply_type, data_length) = struct.unpack(
            ">QLLL", reply)
        LOGGER.debug("NBD reply magic='0x%x' option='%d' reply_type='%d'",
                     magic, option, reply_type)
        assert_protocol(magic == OPTION_REPLY_MAGIC)
        if option != self._last_sent_option:
            raise NBDUnexpectedOptionResponseError(
                expected=self._last_sent_option, received=option)
        if reply_type & NBD_REP_ERROR_BIT != 0:
            raise NBDOptionError(reply=reply_type)
        data = self._recvall(data_length)
        return (reply_type, data)

    def _parse_option_reply_ack(self):
        (reply_type, data) = self._parse_option_reply()
        if reply_type != NBD_REP_ACK:
            raise NBDProtocolError()
        return data

    def _parse_meta_context_reply(self):
        (reply_type, data) = self._parse_option_reply()
        if reply_type == NBD_REP_ACK:
            return None
        assert_protocol(reply_type == NBD_REP_META_CONTEXT)
        context_id = struct.unpack(">L", data[:4])[0]
        name = (data[4:]).decode('utf-8')
        return (context_id, name)

    def _upgrade_socket_to_tls(self, cert, subject):
        # Forcing the client to use TLSv1_2
        context = ssl.SSLContext(ssl.PROTOCOL_TLSv1_2)
        context.options &= ~ssl.OP_NO_TLSv1
        context.options &= ~ssl.OP_NO_TLSv1_1
        context.options &= ~ssl.OP_NO_SSLv2
        context.options &= ~ssl.OP_NO_SSLv3
        context.verify_mode = ssl.CERT_REQUIRED
        context.check_hostname = (subject is not None)
        context.load_verify_locations(cadata=cert)
        cleartext_socket = self._s
        self._s = context.wrap_socket(
            cleartext_socket,
            server_side=False,
            do_handshake_on_connect=True,
            server_hostname=subject)

    def _initiate_tls_upgrade(self):
        # start TLS negotiation
        self._send_option(NBD_OPT_STARTTLS)
        # receive reply
        data = self._parse_option_reply_ack()
        assert_protocol(len(data) == 0)

    def request_info(self, export_name, info_requests):
        """Query information from the server."""
        data = struct.pack('>L', len(export_name))
        data += export_name.encode('utf-8')
        data += struct.pack('>H', len(info_requests))
        for request in info_requests:
            data += struct.pack('>H', request)
        self._send_option(NBD_OPT_INFO, data)
        infos = []
        while True:
            (reply_type, data) = self._parse_option_reply()
            if reply_type == NBD_REP_INFO:
                info_type = struct.unpack(">H", data[:2])[0]
                info = {'information_type': info_type}
                payload = data[2:]
                if info_type == NBD_INFO_BLOCK_SIZE:
                    assert_protocol(len(data) == 14)
                    sizes = struct.unpack('>LLL', payload)
                    (info['minimum_block_size'],
                     info['preferred_block_size'],
                     info['maximum_block_size']) = sizes
                    infos += [info]
                elif info_type == NBD_INFO_EXPORT:
                    assert_protocol(len(data) == 12)
                    export_info = struct.unpack('>QH', payload)
                    (info['size'],
                     info['transmission_flags']) = export_info
                    infos += [info]
                else:
                    # The client MUST ignore information replies it does not
                    # understand.
                    LOGGER.warning('Unsupported info reply type: %d',
                                   info_type)
            elif reply_type == NBD_REP_ACK:
                assert_protocol(not data)
                break
            else:
                raise NBDProtocolError(
                    'Unexpected reply type: {}'.format(reply_type))
        return infos

    def negotiate_structured_reply(self):
        """
        Negotiate use of the structured reply extension, fail if unsupported.
        Only valid during the handshake phase.
        """
        self._send_option(NBD_OPT_STRUCTURED_REPLY)
        self._parse_option_reply_ack()
        self._structured_reply = True

    def _process_meta_context_option(self, option, export_name, queries):
        data = struct.pack('>L', len(export_name))
        data += export_name.encode('utf-8')
        data += struct.pack('>L', len(queries))
        for query in queries:
            data += struct.pack('>L', len(query))
            data += query.encode('utf-8')
        self._send_option(option, data)
        while True:
            reply = self._parse_meta_context_reply()
            if reply is None:
                break
            yield reply

    def _send_meta_context_option(self, option, export_name, queries):
        return list(self._process_meta_context_option(
            option, export_name, queries))

    def set_meta_contexts(self, export_name, queries):
        """
        Change the set of active metadata contexts. Only valid during the
        handshake phase. Returns the list of selected metadata contexts as
        (metadata context ID, metadata context name) pairs.
        Structured replies must be negotiated first using
        negotiate_structured_reply.
        """
        return self._send_meta_context_option(
            option=NBD_OPT_SET_META_CONTEXT,
            export_name=export_name,
            queries=queries)

    def list_meta_contexts(self, export_name, queries):
        """
        Return the metadata contexts available on the export matching one or
        more of the queries as (metadata context ID, metadata context name)
        pairs.
        Structured replies be negotiated first using
        negotiate_structured_reply.
        """
        return self._send_meta_context_option(
            option=NBD_OPT_LIST_META_CONTEXT,
            export_name=export_name,
            queries=queries)

    def _fixed_new_style_handshake(self, cert, subject, use_tls):
        nbd_magic = self._recvall(len("NBDMAGIC"))
        assert_protocol(nbd_magic == b'NBDMAGIC')
        nbd_magic = self._recvall(len("IHAVEOPT"))
        assert_protocol(nbd_magic == b'IHAVEOPT')
        buf = self._recvall(2)
        handshake_flags = struct.unpack(">H", buf)[0]
        assert_protocol(handshake_flags & NBD_FLAG_HAS_FLAGS != 0)
        client_flags = NBD_FLAG_C_FIXED_NEWSTYLE
        client_flags = struct.pack('>L', client_flags)
        self._s.sendall(client_flags)

        if use_tls:
            # start TLS negotiation
            self._initiate_tls_upgrade()
            # upgrade socket to TLS
            self._upgrade_socket_to_tls(cert, subject)

    def connect(self, exportname):
        """
        Valid only during the handshake phase. Requests the given
        export and enters the transmission phase.
        """
        LOGGER.info("Connecting to export '%s' using newstyle negotiation",
                    exportname)
        # request export
        self._send_option(NBD_OPT_EXPORT_NAME, exportname.encode('utf-8'))

        # non-fixed newstyle negotiation: we get these if the server is willing
        # to allow the export
        buf = self._recvall(10)
        (self._size, self._transmission_flags) = struct.unpack(">QH", buf)
        LOGGER.debug("NBD got size=%d transmission flags=%d",
                     self._size, self._transmission_flags)
        # ignore the zeroes
        zeroes = self._recvall(124)
        LOGGER.debug("NBD got zeroes: %s", zeroes)
        self._transmission_phase = True
        LOGGER.debug("Connected")

    #  Oldstyle handshake

    def _old_style_handshake(self):
        LOGGER.info("Connecting to server using oldstyle negotiation")
        nbd_magic = self._recvall(len("NBDMAGIC"))
        assert_protocol(nbd_magic == b'NBDMAGIC')
        buf = self._recvall(8 + 8 + 4)
        (magic,
         self._size,
         self._transmission_flags) = struct.unpack(">QQL", buf)
        assert_protocol(magic == 0x00420281861253)
        # ignore trailing zeroes
        self._recvall(124)
        self._transmission_phase = True

    # Transmission phase

    def _send_request_header(self, request_type, offset, length):
        LOGGER.debug("NBD request offset=%d length=%d", offset, length)
        command_flags = 0
        self._handle += 1
        header = struct.pack('>LHHQQL', NBD_REQUEST_MAGIC, command_flags,
                             request_type, self._handle, offset, length)
        self._s.sendall(header)

    def _check_handle(self, handle):
        if handle != self._handle:
            raise NBDUnexpectedReplyHandleError(
                expected=self._handle, received=handle)

    def _parse_simple_reply(self, data_length=0):
        LOGGER.debug("NBD parsing simple reply, data_length=%d", data_length)
        reply = self._recvall(4 + 4 + 8)
        (magic, errno, handle) = struct.unpack(">LLQ", reply)
        LOGGER.debug("NBD simple reply magic='0x%x' errno='%d' handle='%d'",
                     magic, errno, handle)
        assert_protocol(magic == NBD_SIMPLE_REPLY_MAGIC)
        self._check_handle(handle)
        data = self._recvall(length=data_length)
        LOGGER.debug("NBD response received data_length=%d bytes", data_length)
        if errno != 0:
            raise NBDTransmissionError(errno)
        return data

    def _handle_block_status_reply(self, fields):
        data_length = fields['data_length']
        assert_protocol((data_length >= 12) and (data_length % 8 == 4))
        data = self._recvall(data_length)
        view = memoryview(data)
        fields['context_id'] = struct.unpack(">L", view[:4])[0]
        view = view[4:]
        descriptors = list(_parse_block_status_descriptors(view))
        assert_protocol(descriptors)
        fields['descriptors'] = descriptors

    def _handle_data_reply(self, fields):
        data_length = fields['data_length']
        assert_protocol(data_length >= 9)
        buf = self._recvall(8)
        fields['offset'] = struct.unpack(">Q", buf)[0]
        fields['data'] = self._recvall(data_length - 8)
        assert_protocol(fields['data'])

    def _handle_hole_reply(self, fields):
        assert_protocol(fields['data_length'] == 12)
        buf = self._recvall(12)
        (fields['offset'], fields['hole_size']) = struct.unpack(">QL", buf)

    def _handle_structured_reply_error(self, fields):
        data_length = fields['data_length']
        assert_protocol(data_length >= 6)
        buf = self._recvall(4 + 2)
        (errno, message_length) = struct.unpack(">LH", buf)
        fields['error'] = errno
        remaining_length = data_length - 6
        # The client MAY continue transmission in case of an unexpected error
        # type, unless message_length does not fit into the length:
        if message_length > remaining_length:
            raise NBDProtocolError(
                'message_length is too large to fit within data_length bytes')
        data = self._recvall(remaining_length)
        view = memoryview(data)
        fields['message'] = view[0:message_length].tobytes().decode('utf-8')
        view = view[message_length:]
        if fields['reply_type'] == NBD_REPLY_TYPE_ERROR_OFFSET:
            fields['offset'] = struct.unpack(">Q", view)[0]

    def _parse_structured_reply_chunk(self):
        LOGGER.debug("NBD parsing structured reply chunk")
        reply = self._recvall(4 + 2 + 2 + 8 + 4)
        header = struct.unpack(">LHHQL", reply)
        (magic, flags, reply_type, handle, data_length) = header
        LOGGER.debug("NBD structured reply magic='%x' flags='%s' "
                     "reply_type='%d' handle='%d' data_length='%d'",
                     magic, flags, reply_type, handle, data_length)
        assert_protocol(magic == NBD_STRUCTURED_REPLY_MAGIC)
        self._check_handle(handle)
        fields = {'flags': flags,
                  'reply_type': reply_type,
                  'data_length': data_length}
        if reply_type == NBD_REPLY_TYPE_BLOCK_STATUS:
            self._handle_block_status_reply(fields)
        elif reply_type == NBD_REPLY_TYPE_NONE:
            assert_protocol(data_length == 0)
            assert_protocol(_is_final_structured_reply_chunk(flags=flags))
        elif reply_type == NBD_REPLY_TYPE_OFFSET_DATA:
            self._handle_data_reply(fields)
        elif reply_type == NBD_REPLY_TYPE_OFFSET_HOLE:
            self._handle_hole_reply(fields)
        elif is_error_chunk(reply_type=reply_type):
            self._handle_structured_reply_error(fields)
        else:
            raise NBDUnexpectedStructuredReplyType(reply_type)
        return fields

    def _parse_structured_reply_chunks(self):
        while True:
            reply = self._parse_structured_reply_chunk()
            yield reply
            if _is_final_structured_reply_chunk(flags=reply['flags']):
                return

    def write(self, data, offset):
        """
        Writes the given bytes to the export, starting at the given
        offset.
        """
        LOGGER.debug("NBD_CMD_WRITE")
        _check_alignment("offset", offset)
        _check_alignment("size", len(data))
        self._flushed = False
        self._send_request_header(NBD_CMD_WRITE, offset, len(data))
        self._s.sendall(data)
        # TODO: the server MAY respond with a structured reply (e.g. to report
        # errors)
        self._parse_simple_reply()
        return len(data)

    def read(self, offset, length):
        """
        Returns length number of bytes read from the export, starting at
        the given offset.
        If structured replies have been negotiated, it returns a generator
        containing the reply chunks. The caller must consume this generator
        before further NBD commands, since this client does not support
        asynchronous request processing.
        """
        LOGGER.debug("NBD_CMD_READ")
        _check_alignment("offset", offset)
        _check_alignment("length", length)
        self._send_request_header(NBD_CMD_READ, offset, length)
        if self._structured_reply:
            return self._parse_structured_reply_chunks()
        data = self._parse_simple_reply(length)
        return data

    def _need_flush(self):
        return self._transmission_flags & NBD_FLAG_SEND_FLUSH != 0

    def flush(self):
        """
        Sends a flush request to the server if the server supports it
        and there are unflushed writes. This causes all completed writes
        (the writes for which the server has already sent a reply to the
        client) to be written to permanent storage.
        """
        if self._need_flush() is False:
            self._flushed = True
            return False
        LOGGER.debug("NBD_CMD_FLUSH")
        self._send_request_header(NBD_CMD_FLUSH, 0, 0)
        # TODO: the server MAY respond with a structured reply (e.g. to report
        # errors)
        self._parse_simple_reply()
        self._flushed = True
        return True

    def query_block_status(self, offset, length):
        """
        Query block status in the range defined by length and offset.
        Returns a list of structured reply chunks.
        The required meta contexts must have been negotiated using
        set_meta_contexts.
        """
        LOGGER.debug("NBD_CMD_BLOCK_STATUS")
        self._send_request_header(NBD_CMD_BLOCK_STATUS, offset, length)
        return list(self._parse_structured_reply_chunks())

    def _disconnect(self):
        if self._transmission_phase:
            LOGGER.debug("NBD_CMD_DISC")
            self._send_request_header(NBD_CMD_DISC, 0, 0)
        else:
            self._send_option(NBD_OPT_ABORT)

    def get_size(self):
        """
        Return the size of the device in bytes.
        """
        return self._size
