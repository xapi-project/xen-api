[![Build Status](https://travis-ci.org/xapi-project/xen-api-client.svg?branch=master)](https://travis-ci.org/xapi-project/xen-api-client)
[![Coverage Status](https://coveralls.io/repos/github/xapi-project/xen-api-client/badge.svg?branch=master)](https://coveralls.io/github/xapi-project/xen-api-client?branch=master)

Ocaml client for the Xen API and bindings for `lwt` and `async`.

Check out the examples in `lwt_examples` and `async_examples`. They can be built with `make lwt-examples` or `make async-examples`.

API bindings can be updated to specific versions of xapi by running

```
# This assumes a fully working opam environment
URL=xapi/gihub/repo BRANCH=branch-name make regenerate
```

By default `URL=https://github.com/xapi-project/xen-api` and `BRANCH=master` when running `make regenerate`.
