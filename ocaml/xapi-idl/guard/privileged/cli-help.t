  $ ./xapiguard_cli.exe --help=plain
  NAME
         xapiguard_cli - A CLI for the deprivileged socket spawning API. This
         allows scripting of the varstored and SWTPM deprivileging daemon for
         testing and debugging. This tool is not intended to be used as an end
         user tool
  
  SYNOPSIS
         xapiguard_cli [COMMAND] …
  
  COMMANDS
         varstore_create [OPTION]… dbg vm_uuid gid path
             Create a deprivileged varstore socket that only accepts API calls
             for a specific VM. The socket will be writable only to the
             specified group.
  
         varstore_destroy [OPTION]… dbg gid path
             Stop listening on varstore sockets for the specified group
  
         vtpm_create [OPTION]… dbg vm_uuid gid path
             Create a deprivileged vtpm socket that only accepts API calls for
             a specific VM. The socket will be writable only to the specified
             group.
  
         vtpm_destroy [OPTION]… dbg gid path
             Stop listening on vtpm sockets for the specified group
  
         vtpm_get_contents [OPTION]… dbg vtpm_uuid
             Get vTPM contents blob
  
         vtpm_set_contents [OPTION]… dbg vtpm_uuid string
             Set vTPM contents blob
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         xapiguard_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
