To-do list
==========

  1. Update the transaction coalesce algorithm so that a VIF/VBD device
     add doesn't cause a conflict (see the disabled unit test)

     * DONE

  2. Update the quota system to make it work for driver domains.
     Consider using 'created by' rather than 'owner' for quota?

     * DONE

  3. Switch the parsing/printing over to cstruct, rather than bitstring

     * DONE

  4. Add debug commands to change runtime configuration e.g. turn on read logging

     * DONE

  5. Allow pcap-style packet dumping/tracing

  6. Add a web interface for interactive debugging. This could be a separate
     domain using the debug interface to isolate the TCP stack.

  7. Consider storing Node.children as a map rather than a list

  8. Add quota for generated watch events

     * DONE

  9. Coalesce identical watch events

  10. Add unix xenctrl frontend

     * DONE

  11. Add mirage domain frontend

  12. Remove exceptions
