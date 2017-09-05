#!/usr/bin/python
"""provide a message based protocol for talking over a PV tty"""
from __future__ import print_function
from __future__ import unicode_literals

import sys
import fcntl

import serial

class Tty(object):
    """locking wrapper around a PV TTY"""
    def __init__(self, ttyname, baud):
        self.ttyname = ttyname
        self.baud = baud
        self.serialport = None

    def await_file_lock(self):
        """Block until noone else is using the file"""
        try:
            fcntl.flock(self.serialport.fileno(), fcntl.LOCK_EX)
        except IOError:
            print('Unable to obtain lock', file=sys.stderr)
            sys.exit(1)

    def __enter__(self):
        """Connect to specified tty"""
        self.serialport = serial.Serial()

        # configure serial connection
        self.serialport.port = self.ttyname
        self.serialport.baudrate = self.baud
        self.serialport.bytesize = serial.EIGHTBITS     # number of bits per bytes
        self.serialport.parity = serial.PARITY_NONE     # set parity check: no parity
        self.serialport.stopbits = serial.STOPBITS_ONE  # number of stop bits
        self.serialport.timeout = None      # block read
        # ser.timeout = 1           # non-block read
        # ser.timeout = 2         # timeout block read
        self.serialport.xonxoff = False   # disable software flow control
        self.serialport.rtscts = False    # disable hardware (RTS/CTS) flow control
        self.serialport.dsrdtr = False    # disable hardware (DSR/DTR) flow control
        self.serialport.writeTimeout = 2  # timeout for write

        # start the connection, this will be part of a context manager in the implementation
        try:
            self.serialport.open()
            self.await_file_lock()
            return self.serialport
        except Exception as err:
            print("error opening serial port: " + str(err), file=sys.stderr)
            sys.exit(1)

    def __exit__(self, exittype, value, traceback):
        fcntl.flock(self.serialport.fileno(), fcntl.LOCK_UN)
        self.serialport.close()

class UnicodeStream(object):
    "Turn a byte stream into a stream of valid unicode characters"
    def __init__(self, serialport):
        self.serialport = serialport

    def await_ctrl_code(self, code):
        """Wait for a control code, yielding UTF-8 characters which arrive in the meantime"""
        bytestr = b""
        while True:
            data = self.serialport.read(size=1)
            if isinstance(data, str): #Cope with pythons that return strings rather than bytestrings
                data = ord(data)
            #Ensure we only return complete unicode characters
            #Note that this function assumes we are being given valid
            # UTF-8.  In the event we are not, this function should
            # cope, but will not return an error and the characters
            # returned may be invalid unicode
            if (data & 0xc0) == 0xc0:    #0xc0 indicates the beginning of a multibyte UTF-8 char
                if bytestr:     #so if we were processing a character, it's finished
                    yield bytestr
                bytestr = b"".join([bytestr, bytes(bytearray([data]))])
            elif (data & 0xc0) == 0x80:   #0x80 indicates a subsequent UTF-8 character byte
                bytestr = b"".join([bytestr, bytes(bytearray([data]))]) #so we save it, because we
                                                             #don't know if it's the last
            else:
                if bytestr:         #this is an ascii UTF-8 character
                    yield bytestr   #so any previous characters must be finished
                if data == ord(code):
                    return
                yield b"".join([bytes(bytearray([data]))])
                bytestr = b""


class GuestCommands(object):
    """Call commands on a guest VM, and read response as it arrives"""
    START_MESSAGE = chr(0x02)
    END_MESSAGE = chr(0x03)
    def __init__(self, serialport):
        self.serialport = serialport

    def read_response(self, response_stream):
        """Yield unicode received from a guest command as it arrives"""
        for i in response_stream.await_ctrl_code(self.START_MESSAGE):
            pass

        for i in response_stream.await_ctrl_code(self.END_MESSAGE):
            try:
                yield i.decode('UTF-8')
            except UnicodeDecodeError:
                print("Invalid UTF-8 data detected", file=sys.stderr)
                sys.exit(1)

    def execute(self, command):
        """Execute a command on a guest"""
        try:
            self.serialport.flushInput()   # flush input buffer, discarding all its contents
            self.serialport.flushOutput()  # flush output buffer, aborting current output
                                           # and discard all that is in buffer
            self.serialport.write(self.START_MESSAGE+command+self.END_MESSAGE)

            response_stream = UnicodeStream(self.serialport)

            for i in self.read_response(response_stream):
                yield i
        except:
            print("Failed to send command "+str(sys.exc_info()), file=sys.stderr)
