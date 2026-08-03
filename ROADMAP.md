# fluff Roadmap

## Current Reality

`fluff` is an experimental FortFront-based linter/formatter. It has many rule
modules and tests, but several user-facing workflows are incomplete or have
known regressions.

The project should not claim full Ruff parity until the open CLI/configuration
issues are resolved.

## Principles

- Use FortFront public APIs for parsing, AST traversal, semantics, and source
  locations.
- Do not add a separate Fortran parser.
- Prefer AST/semantic rules over text scanning.
- Treat formatter semantic preservation as higher priority than style polish.
- Keep LSP and cache features behind correctness.

## Priority 0: Formatter Safety

The inline-comment issue [#244](https://github.com/lazy-fortran/fluff/issues/244)
is closed on `main` by the current formatter work. Preserve its regression
coverage when touching formatter attachment rules.

Required outcome:

- formatting must preserve comment attachment and not change executable meaning
- add regression tests for inline comments in declarations, assignments,
  conditionals, continuations, and trailing comments after labels

## Priority 1: Daily CLI Usability

Open issues:

- [#243](https://github.com/lazy-fortran/fluff/issues/243): stdin support
- [#242](https://github.com/lazy-fortran/fluff/issues/242): quiet mode
- [#241](https://github.com/lazy-fortran/fluff/issues/241): statistics
- [#240](https://github.com/lazy-fortran/fluff/issues/240): exclude flag
- [#239](https://github.com/lazy-fortran/fluff/issues/239): select/ignore flags
- [#238](https://github.com/lazy-fortran/fluff/issues/238): diagnostic filename
  spacing
- [#237](https://github.com/lazy-fortran/fluff/issues/237): show-fixes preview
- [#236](https://github.com/lazy-fortran/fluff/issues/236): rule listing
- [#235](https://github.com/lazy-fortran/fluff/issues/235): configuration
  loading regression

Required outcome:

- `fluff check` works predictably in CI
- rule selection and config loading are dependable
- output is scriptable and stable

## Priority 2: Rule Accuracy

Open issue:

- [#77](https://github.com/lazy-fortran/fluff/issues/77): MVP tracking epic

Required outcome:

- each rule documents whether it is text, AST, or semantic based
- false-positive prone performance rules use semantic context where available
- fix suggestions are only offered when the replacement is mechanically safe

## Priority 3: Cache and LSP Polish

Current linter code reparses files because caching AST contexts risks shallow
copying FortFront arena/semantic-context state. Restore caching only after
FortFront exposes safe ownership/reuse semantics for AST contexts.

LSP features should remain thin wrappers around the same check/format engine
used by the CLI.

## Deferred

- plugin ecosystem
- editor packages
- file watching
- broad migration tooling from other linters

These should wait until formatter safety, config, and CLI basics are stable.

## Current handoff (2026-08-03)

- `main` is `b7fdd2a6cfc682eb9eb4b1421924a4d321875dba`.
- [#262](https://github.com/lazy-fortran/fluff/issues/262) remains open and
  [PR #269](https://github.com/lazy-fortran/fluff/pull/269) is the active
  repair branch. Its rebased head is `39045fc`; the branch is pushed but is
  not merge-ready while the enhanced-style and quality-improvement behavioral
  tests remain red in the full pipeline.
- fo [#59](https://github.com/lazy-fortran/fo/issues/59) depends on stable
  combined JSON. Do not hide a failing test or weaken its independent oracle
  to make the JSON consumer green.

The delivery gate is the focused suite plus the full `FO_JOBS=1 fo` pipeline;
all test programs must be able to fail the build for a real behavioral reason.
