#!/usr/bin/env python3
"""
Test script to send raw XML-RPC calls to the varstored-guard Unix socket.
Displays the full response to diagnose 3-param vs 4-param behavior.

Usage (on the host):
  python3 /tmp/test_varstored_socket.py

Requires the varstored depriv socket to exist for a running VM.
"""

import socket
import sys

# Adjust these for your environment
SOCKET_PATH = "/var/run/xen/varstored-root-5/xapi-depriv-socket"
VM_UUID = "db4edfe0-b0e7-c78a-db4b-93d9506d6cae"

# Dummy NVRAM value (small, just for testing)
NVRAM_VALUE = "dGVzdA=="  # base64 of "test"


def send_xmlrpc(socket_path, xml_body):
    """Send an XML-RPC request over a Unix socket and return the raw response."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(socket_path)

        http_request = (
            "POST / HTTP/1.0\r\n"
            "Content-Type: text/xml\r\n"
            "Content-Length: {}\r\n"
            "\r\n"
            "{}"
        ).format(len(xml_body), xml_body)

        sock.sendall(http_request.encode("utf-8"))

        # Read the full response
        response = b""
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            response += chunk

        return response.decode("utf-8", errors="replace")
    finally:
        sock.close()


def make_3param_xml():
    """Old varstored format: session_id, vm_ref, nvram_value (3 params)."""
    return """<?xml version="1.0"?>
<methodCall>
  <methodName>VM.set_NVRAM_EFI_variables</methodName>
  <params>
    <param><value><string>session-dummy</string></value></param>
    <param><value><string>{vm_uuid}</string></value></param>
    <param><value><string>{nvram}</string></value></param>
  </params>
</methodCall>""".format(vm_uuid=VM_UUID, nvram=NVRAM_VALUE)


def make_4param_xml(update="no"):
    """New format: session_id, vm_ref, nvram_value, update (4 params)."""
    return """<?xml version="1.0"?>
<methodCall>
  <methodName>VM.set_NVRAM_EFI_variables</methodName>
  <params>
    <param><value><string>session-dummy</string></value></param>
    <param><value><string>{vm_uuid}</string></value></param>
    <param><value><string>{nvram}</string></value></param>
    <param><value><string>{update}</string></value></param>
  </params>
</methodCall>""".format(vm_uuid=VM_UUID, nvram=NVRAM_VALUE, update=update)


def run_test(label, xml_body):
    print("=" * 60)
    print(f"TEST: {label}")
    print("=" * 60)
    print(f"Request XML:\n{xml_body}\n")
    try:
        response = send_xmlrpc(SOCKET_PATH, xml_body)
        print(f"Response:\n{response}")
    except ConnectionRefusedError:
        print("ERROR: Connection refused. Is varstored-guard running for this VM?")
    except FileNotFoundError:
        print(f"ERROR: Socket not found at {SOCKET_PATH}")
    except Exception as e:
        print(f"ERROR: {e}")
    print()


if __name__ == "__main__":
    print(f"Socket: {SOCKET_PATH}")
    print(f"VM UUID: {VM_UUID}")
    print()

    # Test 1: 3 params (what real varstored sends)
    run_test("3-param call (old varstored format)", make_3param_xml())

    # Test 2: 4 params with update=no
    run_test("4-param call (update=no)", make_4param_xml("no"))

    # Test 3: 4 params with update=yes
    run_test("4-param call (update=yes)", make_4param_xml("yes"))
