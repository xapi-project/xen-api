#!/usr/bin/env python

# Example script which shows how to use the XenAPI to find a particular Host's management interface
# and send it a wake-on-LAN packet.

import subprocess, sys, socket, struct, time, syslog

import XenAPI, inventory

import XenAPIPlugin

class HOST_POWER_ON_NOT_CONFIGURED(Exception):
    """Base Exception class for all transfer plugin errors."""
    def __init__(self, *args):
        Exception.__init__(self, *args)


def waitForXapi(session,host):
    attempts = 0
    finished = False
    metrics = None
    while not finished and (attempts < 120):
        attempts = attempts + 1
        time.sleep(5)
        metrics = session.xenapi.host.get_metrics(host)
        try:
            finished = session.xenapi.host_metrics.get_live(metrics)
        except:
            pass
    return str(finished)


def main(session, args):
    remote_host_uuid = args['remote_host_uuid']
    
        
    # Find the remote Host
    remote_host = session.xenapi.host.get_by_uuid(remote_host_uuid)
    # Find the power_on_mode
    mode = session.xenapi.host.get_power_on_mode(remote_host)
    
    power_on_config = session.xenapi.host.get_power_on_config(remote_host)

    if mode=="DRAC" :
        ip=power_on_config['power_on_ip']
        user = power_on_config['power_on_user']
        secret = power_on_config['power_on_password_secret']
        secretref=session.xenapi.secret.get_by_uuid(secret)
        password = session.xenapi.secret.get_value(secretref)
        modu= __import__('DRAC')
        modu.DRAC( ip, user, password)
        return waitForXapi(session,remote_host)
    elif mode=="wake-on-lan":
        modu= __import__('wlan')
        return modu.wake_on_lan(session, remote_host, remote_host_uuid)
    # Custom script
    elif mode!="":
        modu = None
        try:
            modu = __import__(mode)
        except ModuleNotFoundError as e:
            # iLO.py was removed as part of REQ-811, so tell user why they are receiving this error
            if mode=="iLO":
                syslog.syslog(syslog.LOG_ERR, "iLO script was removed")
            raise e

        modu.custom(session,remote_host, power_on_config)
        return waitForXapi(session,remote_host)
    # Disabled
    else:
        raise HOST_POWER_ON_NOT_CONFIGURED()



if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})

