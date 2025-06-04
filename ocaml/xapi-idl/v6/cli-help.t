  $ ./v6_cli.exe --help=plain
  NAME
         licensing_cli - A CLI for the V6d API. This allows scripting of the
         licensing daemon for testing and debugging. This tool is not intended
         to be used as an end user tool
  
  SYNOPSIS
         licensing_cli [COMMAND] …
  
  COMMANDS
         apply_edition [OPTION]… debug_info string string_pair_lst
             Checks license info to ensures enabled features are compatible.
  
         get_editions [OPTION]… debug_info
             Gets list of accepted editions.
  
         get_version [OPTION]… debug_info
             Gets list of version-related string pairs
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         licensing_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  

