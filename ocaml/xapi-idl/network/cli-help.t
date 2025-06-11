  $ ./network_cli.exe --help=plain
  NAME
         network_cli - A CLI for the network API. This allows scripting of the
         xcp-networkd daemon for testing and debugging. This tool is not
         intended to be used as an end user tool
  
  SYNOPSIS
         network_cli [COMMAND] …
  
  COMMANDS
         Network.Bridge.add_port [OPTION]… string bridge name interfaces
             Add port
  
         Network.Bridge.create [OPTION]… string name
             Create bridge
  
         Network.Bridge.destroy [OPTION]… string force name
             Destroy bridge
  
         Network.Bridge.get_all [OPTION]… string
             Get all bridges
  
         Network.Bridge.get_all_bonds [OPTION]… string from_cache
             get all bonds
  
         Network.Bridge.get_all_ports [OPTION]… string from_cache
             Get all ports
  
         Network.Bridge.get_interfaces [OPTION]… string name
             Get interfaces
  
         Network.Bridge.get_kind [OPTION]… string
             Get backend kind
  
         Network.Bridge.get_physical_interfaces [OPTION]… string name
             Get physical interfaces
  
         Network.Bridge.make_config [OPTION]… string conservative config
             Make bridge configuration
  
         Network.Bridge.remove_port [OPTION]… string bridge name
             Remove port
  
         Network.Bridge.set_persistent [OPTION]… string name value
             Make bridge to persistent or not
  
         Network.Interface.bring_down [OPTION]… string name
             Bring PIF down
  
         Network.Interface.exists [OPTION]… string name
             Check interface existence
  
         Network.Interface.get_all [OPTION]… string
             Get list of all interface names
  
         Network.Interface.get_capabilities [OPTION]… string name
             Get capabilities on the interface
  
         Network.Interface.get_dns [OPTION]… string name
             Get DNS
  
         Network.Interface.get_interface_positions [OPTION]… string
             Get list of interface names and their positions
  
         Network.Interface.get_ipv4_addr [OPTION]… string name
             Get list of IPv4 addresses of the interface
  
         Network.Interface.get_ipv4_gateway [OPTION]… string name
             Get IPv4 gateway
  
         Network.Interface.get_ipv6_addr [OPTION]… string name
             Get IPv6 address
  
         Network.Interface.get_ipv6_gateway [OPTION]… string name
             Get IPv6 gateway
  
         Network.Interface.get_mac [OPTION]… string name
             Get Mac address of the interface
  
         Network.Interface.get_mtu [OPTION]… string name
             Get MTU
  
         Network.Interface.get_pci_bus_path [OPTION]… string name
             Get PCI bus path of the interface
  
         Network.Interface.has_vlan [OPTION]… string name vlan
             Check whether interface has vlan
  
         Network.Interface.is_connected [OPTION]… string name
             Check whether interface is connected
  
         Network.Interface.is_physical [OPTION]… string name
             Check whether interface is physical
  
         Network.Interface.is_up [OPTION]… string name
             Check whether the interface is up
  
         Network.Interface.make_config [OPTION]… string conservative config
             Make interface configuration
  
         Network.Interface.set_ipv4_conf [OPTION]… string name ipv4
             Set IPv4 configuration
  
         Network.Interface.set_persistent [OPTION]… string name value
             Make PIF to persistent or not
  
         Network.PVS_proxy.configure_site [OPTION]… string t
             Configure site
  
         Network.PVS_proxy.remove_site [OPTION]… string string
             Remove site
  
         Network.Sriov.disable [OPTION]… string name
             Disable SR-IOV
  
         Network.Sriov.enable [OPTION]… string name
             Enable SR-IOV
  
         Network.Sriov.make_vf_config [OPTION]… string address sriov_pci_t
             Make SR-IOV vf config
  
         Network.clear_state [OPTION]…
             Clear configuration state then lock the writing of the state to
             disk
  
         Network.reset_state [OPTION]…
             Reset configuration state
  
         Network.set_dns_interface [OPTION]… string name
             Set dns interface
  
         Network.set_gateway_interface [OPTION]… string name
             Set gateway interface
  
         Network.sync_state [OPTION]…
             Allow for the config state to be written to disk then perform a
             write
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         network_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  

