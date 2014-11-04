All APIs evolve as bugs are fixed, new features added and features are removed
- the XenAPI is no exception. This document lists policies describing how the
XenAPI evolves over time.

The goals of XenAPI evolution are:
- to allow bugs to be fixed efficiently;
- to allow new, innovative features to be added easily;
- to keep old, unmodified clients working as much as possible; and
- where backwards-incompatible changes are to be made, publish this
  information early to enable affected parties to give timely feedback.

Background
==========

In this document, the term *XenAPI* refers to the XMLRPC-derived wire protocol
used by xapi. The XenAPI has *objects* which each have *fields* and
*messages*. The XenAPI is described in detail elsewhere.

XenAPI Lifecycle
================

![API lifecycle states](http://xapi-project.github.io/xen-api/doc/design/XenAPI-lifecycle.svg)

Each element of the XenAPI (objects, messages and fields) follows the lifecycle
diagram above (inspired by
[symbian](http://developer.symbian.org/wiki/index.php/Public_API_Change_Control_Process)
).
When an element is newly created and being still in development, it is in the
*Prototype* state. Elements in this state may be stubs: the interface is there
and can be used by clients for prototyping their new features, but the actual
implementation is not yet ready.

When the element subsequently becomes ready for use (the stub is replaced by a
real implementation), it transitions to the *Published* state. This is the only
state in which the object, message or field should be used. From this point
onwards, the element needs to have clearly defined semantics that are available
for reference in the XenAPI documentation.

If the XenAPI element becomes *Deprecated*, it will still function as it did
before, but its use is discouraged. The final stage of the lifecycle is the
*Removed* state, in which the element is not available anymore.

The numbered state changes in the diagram have the following meaning:

1. Publish: declare that the XenAPI element is ready for people to use.
2. Extend: a *backwards-compatible*extension of the XenAPI, for example an
   additional parameter in a message with an appropriate default value. If the
   API is used as before, it still has the same effect.
3. Change: a *backwards-incompatible* change. That is, the message now behaves
   differently, or the field has different semantics. Such changes are
   discouraged and should only be considered in special cases (always consider
   whether deprecation is a better solution). The use of a message can for
   example be restricted for security or efficiency reasons, or the behaviour
   can be changed simply to fix a bug.
4. Deprecate: declare that the use of this XenAPI element should be avoided from
   now on. Reasons for doing this include: the element is redundant (it
   duplicates functionality elsewhere), it is inconsistent with other parts of
   the XenAPI, it is insecure or inefficient (for examples of deprecation
   policies of other projects, see
   [symbian](http://developer.symbian.org/wiki/index.php/Public_API_Change_Control_Process)
   [sun](http://java.sun.com/j2se/1.4.2/docs/guide/misc/deprecation/deprecation.html)
   [eclipse](http://wiki.eclipse.org/Eclipse/API_Central/Deprecation_Policy)
   [oval](http://oval.mitre.org/language/about/deprecation.html).
5. Remove: the element is taken out of the public API and can no longer be used.

Each lifecycle transition must be accompanied by an explanation describing the
change and the reason for the change. This message should be enough to
understand the semantics of the XenAPI element after the change, and in the case
of backwards-incompatible changes or deprecation, it should give directions
about how to modify a client to deal with the change (for example, how to avoid
using the a deprecated field or message).

Releases
--------

Every release must be accompanied by *release notes* listing all objects, fields
and messages that are newly prototyped, published, extended, changed, deprecated
or removed in the release. Each item should have an explanation as implied
above,  documenting the new or changed XenAPI element. The release notes for
every release shall be prominently displayed in the XenAPI HTML documentation.

Documentation
-------------


The XenAPI documentation will contain its complete lifecycle history for each
XenAPI element. Only the elements described in the documentation are
"official" and supported.

Each object, message and field in ```datamodel.ml``` will have lifecycle
metadata attached to it, which is a list of transitions (transition type *
release * explanation string) as described above. Release notes are automatically generated from this data.
