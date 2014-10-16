Xenopsd design details
======================

Before reading these, first familiarise yourself with the
[architecture](../architecture/README.md).

- Metadata
  - Registered VMs
  - Backend-private data
- Task handling
  - Cancellation: including discussion of the necessary invariants and testing
    techniques
- Event handling
  - Principles: level-triggered
  - Watching xenstore
  - The 'domain action request'
- Suspend/resume/migrate
  - Discussion of the [needs of the suspend image format](suspend-image-considerations.md)
  - The [suspend image framing format](suspend-image-framing-format.md)
