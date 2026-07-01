# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

xen-api (xapi) is the management control plane for the Xen hypervisor, written primarily in OCaml. It exposes the XenAPI interface consumed by XenServer and related tooling. The codebase is a monorepo containing 80+ opam packages across ~50 OCaml subdirectories.

## Development Setup

OCaml version is pinned in CI to 4.14.2 (via xs-opam). The `opam exec --` prefix is needed when running commands outside the opam switch.

## Commands

```sh
make               # Full build
make test          # Run all unit tests
make format        # Format OCaml code with ocamlformat
```

Run a specific dune test target:
```sh
dune runtest ocaml/tests/
dune exec ocaml/tests/test_vm_helpers.exe
```

## Architecture

### IDL → Code Generation → Implementation

The core pattern of this codebase:

1. **`ocaml/idl/datamodel*.ml`** — defines the entire XenAPI: every class, field, message (method), parameter, and RPC. This is the authoritative schema.

2. **`ocaml/idl/gen_api_main.exe`** — reads the datamodel and generates OCaml, JSON, Markdown, and SDK bindings. Backends live under `ocaml/idl/ocaml_backend/`, `json_backend/`, `markdown_backend/`.

3. **Generated files** (checked in, regenerated with `make schema`):
   - `server.ml` — RPC dispatch table, one branch per message
   - `db_actions.ml` — typed getters/setters for every DB field
   - `custom_actions.ml` — stubs; real implementations go in `ocaml/xapi/`
   - `rbac_static.ml` — RBAC permission map

4. **`ocaml/xapi/`** — the actual daemon. For every message defined in the IDL, a corresponding OCaml function must exist here (the linker enforces this). The entry point is `ocaml/xapi/xapi.ml`.

**Adding a new API call:** add the message to `datamodel*.ml`, run `make schema`, then implement the function in `ocaml/xapi/`. The build will fail until the implementation exists.

### Key Subdirectories

| Path | Role |
|------|------|
| `ocaml/xapi/` | Main daemon — API implementations, startup, pool/host logic |
| `ocaml/idl/` | Schema definition and code generators |
| `ocaml/xapi-idl/` | IDL interfaces for sub-daemons (storage, network, xenopsd, RRD, cluster) |
| `ocaml/libs/` | Supporting libraries (DB engine, auth, HTTP, xenopsd client, etc.) |
| `ocaml/xenopsd/` | VM lifecycle daemon (start/stop/migrate/suspend) |
| `ocaml/xapi-guard/` | Mediates access between VMs and xapi |
| `ocaml/message-switch/` | Inter-daemon message queue |
| `ocaml/xcp-rrdd/` | Performance data collection daemon |
| `ocaml/xapi-storage/` | Storage plugin system (Python scripts + OCaml coordinator) |
| `ocaml/tests/` | Unit tests (Alcotest + QCheck) |

### Inter-daemon Communication

Daemons communicate via `message-switch` (an internal message queue) using typed IDL-defined interfaces from `ocaml/xapi-idl/`. Each sub-daemon exposes an RPC interface; xapi calls these interfaces rather than managing resources directly (e.g., xapi asks xenopsd to start a VM; xenopsd handles Xen hypercalls).

### Database

Xapi uses a custom in-memory XML database (`ocaml/libs/db-tools/`, `ocaml/libs/xapi-db-process/`). Schema is derived from the IDL. `db_actions.ml` provides the typed access layer. DB state is persisted to disk and replicated across pool members.

### Feature Flags

Conditional or experimental code uses feature flags defined in `ocaml/xapi/xapi_globs.ml` and toggled via `xapi.conf`. Prefer this over compile-time conditionals for features that need field-enabling.

## Conventions

**Commits:** Subject line must start with the component name, e.g.:
```
xapi: Fix VM.start race with pool join
xenopsd: Handle SIGTERM during migration
```

Each commit must build and pass `make test` independently.

**DCO:** All commits require `Signed-off-by: Name <email>` (enforced by CI).

**Formatting:** `ocamlformat` with project-local `.ocamlformat`. Run `make format` before pushing; CI will reject unformatted code.

**Testing:** Unit tests use Alcotest. New message implementations should have tests under `ocaml/tests/`. Integration tests may be private but should be described in the PR.

**IDL hash:** After editing any `datamodel*.ml` file, run `make schema` to update the generated hash. CI fails if the hash is stale.
