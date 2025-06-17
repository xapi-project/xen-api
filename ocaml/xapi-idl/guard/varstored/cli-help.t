  $ ./varstored_cli.exe --help=plain
  NAME
         varstored_cli - debug CLI
  
  SYNOPSIS
         varstored_cli [COMMAND] …
  
  COMMANDS
         VM.get_NVRAM [--socket=SOCKET] [OPTION]… string string
             Get the current VM's NVRAM contents
  
         VM.get_by_uuid [--socket=SOCKET] [OPTION]… string string
             Dummy, for wire compatibility with XAPI
  
         VM.set_NVRAM_EFI_variables [--socket=SOCKET] [OPTION]… string string
         string
             Set the current VM's NVRAM contents
  
         message.create [--socket=SOCKET] [OPTION]… string string int64
         string string string
             Send an alert when booting a UEFI guest fails
  
         session.login_with_password [--socket=SOCKET] [OPTION]… string
         string string string
             Dummy, for wire compatibility with XAPI
  
         session.logout [--socket=SOCKET] [OPTION]… string
             Dummy, for wire compatibility with XAPI
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         varstored_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
