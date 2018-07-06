## 3.0.1 (2017-06-17):
* Change dependency on io-page.unix into io-page-unix

## 3.0.0 (2017-05-28):
* Split into 2 opam and ocamlfind packages:
  xen-gnt: same as before
  xen-gnt-unix: this used to be called xen-gnt.unix
* Add dependencies for archlinux
* Switch to jbuilder

## 2.2.3 (2016-11-21):
* Modernise opam file
* Use a multi-distro travis configuration
* Remove lwt.syntax
* Use Array.make instead of Array.create

## 2.2.2 (2016-01-27):
* Add support for Xen-4.7

## 2.2.1 (2015-12-16):
* Start `gntref` counter at zero. (#19)
  Before, the initialisation code decreased the counter for each
  page added to the free list, causing the counter to start negative.

## 2.2.0 (2015-01-27):
* Support the `Io_page` 1.3.0+ API (#17).

## 2.1.0 (2014-12-17):
* Report profiling/tracing information using `mirage-profile` (#15).

## 2.0.0 (2014-10-12):
* Add support for Gnttab and Gntshr variants that may either
  allocate pages or not.  This library now supports both variants.
* Add a local `opam` file for supporting the OPAM 1.2.0 workflow.

## 1.0.3 (2014-08-29):
 * Add workaround for broken Linux xc_gntshr_munmap

## 1.0.2 (2014-06-19):
 * Fix build for Xen, broken in 1.0.1

## 1.0.1 (2014-06-18):
 * This code is now part of the Mirage project
 * better error message when kernel modules are missing
 * standardise on Mirage ISC license

## 1.0.0 (2014-01-30):
 * only compile the C stubs if the xen headers are present. They
   aren't needed for Mirage kernels.

## 0.9.9 (2014-01-30):
 * replace with the Mirage implementation. This allows the same
   application to be relinked to run in userspace or kernelspace.

## 0.5.0 (2013-12-20):
 * initial release.
