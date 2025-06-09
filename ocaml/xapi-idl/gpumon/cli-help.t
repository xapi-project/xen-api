  $ ./gpumon_cli.exe --help=plain
  NAME
         gpumon_cli - A CLI for the GPU monitoring API. This allows scripting
         of the gpumon daemon for testing and debugging. This tool is not
         intended to be used as an end user tool
  
  SYNOPSIS
         gpumon_cli [COMMAND] …
  
  COMMANDS
         get_pgpu_metadata [OPTION]… debug_info pgpu_address
             Gets the metadata for a pGPU, given its address (PCI bus ID).
  
         get_pgpu_vgpu_compatibility [OPTION]… debug_info
         nvidia_pgpu_metadata nvidia_vgpu_metadata_list
             Checks compatibility between a pGPU (on a host) and a list of
             vGPUs (assigned to a VM). Note: A VM may use several vGPUs. The
             use case is VM.suspend/VM.resume: before VM.resume
             [nvidia_vgpu_metadata] of the suspended VM is checked against the
             [nvidia_pgpu_metadata] on the host where the VM is resumed.
  
         get_pgpu_vm_compatibility [OPTION]… debug_info pgpu_address domid
         nvidia_pgpu_metadata
             Checks compatibility between a VM's vGPU(s) and another pGPU.
  
         get_vgpu_metadata [OPTION]… debug_info domid pgpu_address vgpu_uuid
             Obtains metadata for all vGPUs running in a domain.
  
         nvml_attach [OPTION]… debug_info
             Attach nVidia cards to Gpumon for metrics and compatibility
             checking.
  
         nvml_detach [OPTION]… debug_info
             Detach nVidia cards from Gpumon
  
         nvml_is_attached [OPTION]… debug_info
             Return true if nVidia cards are currently attached.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         gpumon_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
