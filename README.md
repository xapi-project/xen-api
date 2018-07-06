[![Build Status](https://travis-ci.org/xapi-project/xapi-test-utils.svg?branch=master)](https://travis-ci.org/xapi-project/xapi-test-utils)

This framework should be useful if you want to apply lots of different
inputs to a function or system, and check that the output is correct in
each case.

The basic idea is to either:

1. Create a module of type `STATELESS_TEST` or
2. Create a module of type `STATEFUL_TEST` and turn it into a `STATELESS_TEST`
with the `EncapsulateState` functor

and then pass the `STATELESS_TEST` into the Make functor.

This gives you a module with two values:
* `test_equal`, a function which takes an input and expected output,
  and tests that the actual output equals the expected output.
* `tests`, which has type `OUnit.test list`. This is a list of tests
  corresponding to the input/output pairs specified as tests in the
  original `STATELESS_TEST` or `STATEFUL_TEST` module, and can be integrated
  into the main OUnit suite.
