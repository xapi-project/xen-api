open Client

let get_set_config_calls_lst =
  Client.[
    ("PBD", PBD.get_all, PBD.get_other_config, PBD.set_other_config, `RW) ;
    ("VBD", VBD.get_all, VBD.get_other_config, VBD.set_other_config, `RW) ;
    ("VDI", VDI.get_all, VDI.get_other_config, VDI.set_other_config, `RW) ;
    ("SR", SR.get_all, SR.get_other_config, SR.set_other_config, `RW) ;
    ("SM", SM.get_all, SM.get_other_config, SM.set_other_config, `RW) ;
    ("PIF", PIF.get_all, PIF.get_other_config, PIF.set_other_config, `RW) ;
    ("VIF", VIF.get_all, VIF.get_other_config, VIF.set_other_config, `RW) ;
    ("VM", VM.get_all, VM.get_other_config, VM.set_other_config, `RW) ;
    ("Network", Network.get_all, Network.get_other_config, Network.set_other_config, `RW) ;
    ("Pool", Pool.get_all, Pool.get_other_config, Pool.set_other_config, `RW) ;
    ("Host", Host.get_all, Host.get_other_config, Host.set_other_config, `RW) ;
    ("PCI", PCI.get_all, PCI.get_other_config, PCI.set_other_config, `RW) ;
    ("Host_cpu", Host_cpu.get_all, Host_cpu.get_other_config, Host_cpu.set_other_config, `RW) ;
    ("GPU_group", GPU_group.get_all, GPU_group.get_other_config, GPU_group.set_other_config, `RW) ;
    ("PGPU", PGPU.get_all, PGPU.get_other_config, PGPU.set_other_config, `RW) ;
    ("Secret", Secret.get_all, Secret.get_other_config, Secret.set_other_config, `RW) ;
  ]

let add_rm_config_calls_lst =
  Client.[
    ("VBD", VBD.get_all, VBD.get_other_config, VBD.add_to_other_config, VBD.remove_from_other_config) ;
    ("VDI", VDI.get_all, VDI.get_other_config, VDI.add_to_other_config, VDI.remove_from_other_config) ;
    ("SR", SR.get_all, SR.get_other_config, SR.add_to_other_config, SR.remove_from_other_config) ;
    ("SM", SM.get_all, SM.get_other_config, SM.add_to_other_config, SM.remove_from_other_config) ;
    ("VIF", VIF.get_all, VIF.get_other_config, VIF.add_to_other_config, VIF.remove_from_other_config) ;
    ("PBD", PBD.get_all, PBD.get_other_config, PBD.add_to_other_config, PBD.remove_from_other_config) ;
    ("PIF", PIF.get_all, PIF.get_other_config, PIF.add_to_other_config, PIF.remove_from_other_config) ;
    ("PGPU", PGPU.get_all, PGPU.get_other_config, PGPU.add_to_other_config, PGPU.remove_from_other_config) ;
    ("Host", Host.get_all, Host.get_other_config, Host.add_to_other_config, Host.remove_from_other_config) ;
  ]
