API for configuring the udhcp server in Dom0
============================================

Summary
-------

This API allows you to configure the DHCP service running on the Host 
Internal Management Network (HIMN). The API configures a udhcp daemon 
residing in Dom0 and alters the service configuration for any VM using 
the network.

It should be noted that for this reason, that callers who modify the
default configuration should be aware that their changes may have an
adverse affect on other consumers of the HIMN.

Version history
---------------

    Date        State
    ----        ----
    2013-3-15   Stable

_Stable_: this API is considered stable and unlikely to change between
software version and between hotfixes.

API description
---------------

The API for configuring the network is based on a series of other_config
keys that can be set by the caller on the HIMN XAPI network object. Once
any of the keys below have been set, the caller must ensure that any VIFs
attached to the HIMN are removed, destroyed, created and plugged.

    ip_begin

The first IP address in the desired subnet that the caller wishes the
DHCP service to use.

    ip_end

The last IP address in the desired subnet that the caller wishes the
DHCP service to use.

    netmask

The subnet mask for each of the issues IP addresses.

    ip_disable_gw

A boolean key for disabling the DHCP server from returning a default
gateway for VMs on the network. To disable returning the gateway address
set the key to True.

_Note_: By default, the DHCP server will issue a default gateway for
those requesting an address. Setting this key may disrupt applications
that require the default gateway for communicating with Dom0 and so
so should be used with care.



Example code
------------

An example python extract of setting the config for the network:

    def get_himn_ref():
        networks = session.xenapi.network.get_all_records()
        for ref, rec in networks.iteritems():
            if 'is_host_internal_management_network' \
                                            in rec['other_config']:                                            
                return ref

        raise Exception("Error: unable to find HIMN.")


    himn_ref = get_himn_ref()
    other_config = session.xenapi.network.get_other_config(himn_ref)
    
    other_config['ip_begin'] = "169.254.0.1"
    other_config['ip_end'] = "169.254.255.254"
    other_config['netmask'] = "255.255.0.0"
    
    session.xenapi.network.set_other_config(himn_ref, other_config)


An example for how to disable the server returning a default gateway:

    himn_ref = get_himn_ref()
    other_config = session.xenapi.network.get_other_config(himn_ref)

    other_config['ip_disable_gw'] = True

    session.xenapi.network.set_other_config(himn_ref, other_config)
