### v1.4.0 (2017-06-23)

* mirage-flow-unix: add `Mirage_flow_unix.Fd` to wrap `Lwt_unix.file_descr` into
  a MirageOS flow (#34, #36, @samoht)
* mirage-flow-lwt: add first class flow values of type `Mirage_flow_lwt.t`
  (#35, @samoht)

### v1.3.0 (2017-06-12)

* Port to Jbuilder (#32 @djs55)

### v1.2.0 (2016-12-21)

* Import `V1.FLOW` from `mirage-types` into `Mirage_flow.S` (@samoht)
* Import `V1_LWT.FLOW` from `mirage-types-lwt` into `Mirage_flow_lwt.S` (@samoht)
* Rename the existing `Mirage_flow` into `Mirage_flow_lwt` (@samoht)
* Rename `Lwt_io_flow` into `Mirage_flow_unix` (@samoht)

### v1.1.0 (2016-01-27)

* Add a new top-level interface `module Mirage_flow`. Existing `module Fflow`
  is still present.
* Add `Mirage_flow.copy` to copy all the data in a flow to another
* Add `Mirage_flow.proxy` to copy data bidirectionally between two flows

### v1.0.3 (2015-07-29)

* Support lwt 2.5.0

### v1.0.2 (2015-06-30)

* Add explicit dependency to OUnit

### v1.0.1 (2015-04-28)

* Add `Fflow.error_message` to satisfay `mirage-types.2.3.0`

### v1.0.0 (2015-02-26)

* Add `Fflow` (functional flows)
* Add `Lwt_io_flow` to convert between Mirage and Lwt flows
