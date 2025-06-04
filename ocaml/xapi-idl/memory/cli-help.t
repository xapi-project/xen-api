  $ ./memory_cli.exe --help=plain
  NAME
         memory_cli - A CLI for the memory API. This allows scripting of the
         squeeze daemon for testing and debugging. This tool is not intended to
         be used as an end user tool
  
  SYNOPSIS
         memory_cli [COMMAND] …
  
  COMMANDS
         balance_memory [OPTION]… string
             Forces a rebalance of the hosts memory. Blocks until the system is
             in a stable state.
  
         delete_reservation [OPTION]… string string reservation_id
             Deletes a reservation. Note that memory rebalancing is not done
             synchronously after the operation has completed.
  
         get_diagnostics [OPTION]… string
             Gets diagnostic information from the server
  
         get_domain_zero_policy [OPTION]… string
             Gets the ballooning policy for domain zero.
  
         get_host_initial_free_memory [OPTION]… string
             Gets the amount of initial free memory in a host
  
         get_host_reserved_memory [OPTION]… string
             Gets the amount of reserved memory in a host. This is the lower
             limit of memory that squeezed will ensure remains unused by any
             domain or reservation.
  
         login [OPTION]… string string
             Logs into the squeeze daemon. Any reservations previously made
             with the specified service name not yet associated with a domain
             will be removed.
  
         query_reservation_of_domain [OPTION]… string string int
             Queries the reservation_id associated with a domain
  
         reserve_memory [OPTION]… string string int64
             [reserve_memory dbg session size] reserves memory for a domain. If
             necessary, other domains will be ballooned down to ensure [size]
             is available. The call returns a reservation_id that can later be
             transferred to a domain.
  
         reserve_memory_range [OPTION]… string string int64 int64
             [reserve_memory_range dbg session min max] reserves memory for a
             domain. If necessary, other domains will be ballooned down to
             ensure enough memory is available. The amount of memory will be
             between [min] and [max] according to the policy in operation. The
             call returns a reservation_id and the actual memory amount that
             can later be transferred to a domain.
  
         transfer_reservation_to_domain [OPTION]… string string
         reservation_id int
             Transfers a reservation to a domain. This is called when the
             domain has been created for the VM for which the reservation was
             initially made.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         memory_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
