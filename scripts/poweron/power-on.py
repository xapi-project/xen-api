#!/usr/bin/env python

# Example script which shows how to use the XenAPI to find a particular Host's management interface
# and send it a wake-on-LAN packet.

import subprocess, sys, socket, struct, time, syslog

import XenAPI, inventory

import XenAPIPlugin



def main(session, args):
    remote_host_uuid = args['remote_host_uuid']
    
        
    # Find the remote Host
    remote_host = session.xenapi.host.get_by_uuid(remote_host_uuid)
    # Find the power_on_mode
    mode = session.xenapi.host.get_power_on_mode(remote_host)
    
    power_on_config = session.xenapi.host.get_power_on_config(remote_host)
    
    if mode == "iLO" or mode=="DRAC" :
        ip=power_on_config['power_on_ip']
        user = power_on_config['power_on_user']
        secret = power_on_config['power_on_password_secret']
        secretref=session.xenapi.secret.get_by_uuid(secret)
        password = session.xenapi.secret.get_value(secretref)
        
        if mode == "iLO":
            modu= __import__('iLO')
            return modu.iLO( ip, user, password)
        else: 
            modu= __import__('DRAC')
            return modu.DRAC(ip, user, password)
    elif mode=="wake-on-lan":
        modu= __import__('wlan')
        return modu.wake_on_lan(session, remote_host, remote_host_uuid)
    # Custom script
    elif mode!="":
        modu= __import__(mode)
        return modu.custom(session,remote_host,power_on_config)
    # Disabled
    else: 
        return str(False)



if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})

