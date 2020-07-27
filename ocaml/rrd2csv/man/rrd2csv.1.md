% RRD2CSV(1) RRD2CSV User Manual | Version 1.0
% Citrix Systems
% September 20, 2012

# NAME

rrd2csv - extract RRDs from xcp-rrdd in the CSV format

# SYNOPSIS

rrd2csv [*options*] [*datasource-descriptor*]*

# DESCRIPTION

rrd2csv is a tool to output XenCloudPlatform and XenServer RRD values
in CSV format. It runs inside dom0, and by default displays all active
datasources that RRDD maintains, but it is also possible to select
datasources of interest as command-line arguments.

*Datasources* (or *DS*) are RRD concepts. They are objects that have
measurements associated with them. They include a metric, a source
from which the metric originate, and a consolidation function. For
instance, if you have multiple VMs, you will have as many
"memory_target" datasources as you have VMs.

# OPTIONS

-n
:   Show name labels instead of UUIDs.

-s *SECONDS*
:    Specify the sampling period, i.e. the time that rrd2csv will wait
     before displaying a new line of values. Note that if you specify a
     sampling period smaller than the period of the data sources, the
     first line of output will be a warning indicating that you will
     get redundant data lines.

-u
:   Show UUIDs as well as name labels.

-v, \--version
:   Print version.

-h, \--help
:   Show usage message.

# DATASOURCE DESCRIPTOR

```
DS     ::= CF ":" SOURCE ":" UUID ":" METRIC
CF     ::= "AVERAGE" | "MIN" | "MAX" | "LAST"
SOURCE ::= "host" | "vm"
UUID   ::= string, e.g. "6266c08b-1992-696e-80ed-68ebdd4c36f4"
METRIC ::= string, e.g. "memory_target"
```

UUID is the XenAPI's UUID of the source and METRIC the metric name. If
you omit any of the fields, this will act as a wildcard. As an
example, specifying ":::" will select all enabled datasources, whereas
":vm::" will select only the datasources related to VMs. Multiple
datasources can be specified on the command line; the result will be
the union of individual filter results.

# EXAMPLES
        
Select host related metrics:

    $ rrd2csv :host::
       
Select VM related metrics:

    $ rrd2csv :vm::
        
Select host metrics that use AVERAGE as consolidation function and
any metric that has name "memory_target" and MIN as consolidation
function:

    $ rrd2csv AVERAGE:host:: MIN:::memory_target



# OUTPUT FORMAT

```
headers
timestamp_0, value_0_0, …, value_0_n
…
timestamp_n, value_n_0, …, value_n_n
```

where

headers
:       comma-separated list of datasources
timestamp 
:       time in unix format when the data has been aquired
value 
:       floating-point numbers, or integers
