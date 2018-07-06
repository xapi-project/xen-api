ppx_typerep_conv
================

Automatic generation of runtime types from type definitions.

This syntax extension defines the type-conv generator `[@@deriving typerep]`, which
creates a (runtime) value (called `typerep_of_$typename`) representing the type definition
(see `typerep` for more information). It is intended to be the main creator of values of
type `Typerep.t`.

This generator supports mostly core types, not all fancy types like union of polymorphic
variants.
