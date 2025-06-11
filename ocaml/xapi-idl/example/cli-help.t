  $ ./example.exe --help=plain
  NAME
         Example-service
  
  SYNOPSIS
         Example-service [OPTION]…
  
  DESCRIPTION
         This is an example service which demonstrates the configuration
         mechanism.
  
  OPTIONS
         --config=VAL (absent=/etc/example.exe.conf)
             Location of configuration file
  
         --config-dir=VAL (absent=/etc/example.exe.conf.d)
             Location of directory containing configuration file fragments
  
         --default-format=VAL (absent=vhd)
             Default format for disk files
  
         --disable-logging-for=VAL
             A space-separated list of debug modules to suppress logging from
  
         --inventory=VAL (absent=/etc/xensource-inventory)
             Location of the inventory file
  
         --log=VAL (absent=syslog:daemon)
             Where to write log messages
  
         --loglevel=VAL (absent=debug)
             Log level
  
         --ls=VAL (absent=/bin/ls)
             program used to list things
  
         --pidfile=VAL (absent=/var/run/example.exe.pid)
             Filename to write process PID
  
         --queue-name=VAL (absent=org.xen.xapi.ffs)
             Comma-separated list of queue names to listen on
  
         --search-path=VAL
             Search path for resources
  
         --sh=VAL (absent=/bin/sh)
             interpreter for arcane programming language
  
         --socket-path=VAL (absent=/var/xapi/socket)
             Path of listening socket
  
         --sr-mount-path=VAL (absent=/mnt)
             Default mountpoint for mounting remote filesystems
  
         --switch-path=VAL (absent=/var/run/message-switch/sock)
             Unix domain socket path on localhost where the message switch is
             listening
  
         --timeslice=VAL (absent=0.05)
             timeslice in seconds
  
         --use-switch=VAL (absent=true)
             true if the message switch is to be enabled
  
  COMMON OPTIONS
         These options are common to all services.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         Example-service exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at http://github.com/xapi-project/xen-api
  

