Service domain message broker
=============================

Aims:
  1. To map persistent well-known names to abstract endpoint identifiers
  2. To monitor the accessibility of each registered endpoint
  3. To route binary messages to a well-known name

Endpoints
---------

An endpoint is a transient address identifier of the following form:

    type idc_endpoint =
       | IP of string (* used for the HIMN *)
       | Vchan of unit
       | V4v of unit

    type domid = int
    type pid = int
    type unix_domain_socket_path = string

    type endpoint =
       | Process of unix_domain_socket_path * pid
       | Domain of idc_endpoint * domid

