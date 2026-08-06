# fluff roadmap

Snapshot: 2026-08-06. fluff is the FortFront-based semantic linter and
source-preserving formatter. It owns AST-dependent analysis. fo owns the cheap
text-level tier and invokes fluff through stable structured output.

## Current truth

The audited baseline is `f122ab7`. Main CI is green in
[run 30807031396](https://github.com/lazy-fortran/fluff/actions/runs/30807031396).
Former CLI/configuration issues #235 through #244 are closed. PR #269 was
closed unmerged. Its relevant repairs were applied directly on main.

Two issues remain open:

- [#262](https://github.com/lazy-fortran/fluff/issues/262): prove every test
  program can make the build fail for a real behavioral mismatch.
- [#77](https://github.com/lazy-fortran/fluff/issues/77): the broad MVP epic.

Green CI is necessary but does not by itself close #262. A test whose negative
control cannot fail is not an oracle.

## Immediate order

1. For each test executable, inject or select a known bad input/expected result
   and prove a nonzero test/build result. Record which assertion failed.
2. Close #262 only when the full set of negative controls is automated and the
   ordinary suite remains green.
3. Freeze versioned JSON records for diagnostics, fixes, source revision, rule
   identity, severity, and tool revision.
4. Complete fo [#59](https://github.com/lazy-fortran/fo/issues/59) against that
   schema with an end-to-end process oracle, including failure propagation and
   output larger than fo's former limits.
5. Continue #77 by false-positive/correctness yield, with formatter semantic
   preservation ahead of style or editor polish.

## Architecture and reliability

- Use public FortFront parsing, semantic, identity, and source-location APIs.
  Do not add a second parser or depend on private arena layout.
- One check/format engine serves CLI, JSON, and later LSP surfaces. Translation
  layers cannot change rule outcomes or suppress failures.
- Formatter edits preserve tokens, comments, continuation structure, labels,
  and executable meaning. A proposed fix is emitted only when mechanical
  application is safe.
- AST caching waits for an explicit immutable snapshot ownership contract from
  FortFront. Never shallow-copy arena/semantic state to save parse time.
- Rule timing, selected rule set, source digest, and cache decision are visible
  so performance work can be reproduced.

## Delivery gates

Every rule needs positive and negative examples plus an independent semantic
oracle where it can affect meaning. Formatter changes compile and run the
original and formatted program with an independent compiler and compare
behavior.

Run focused tests while editing and the full `FO_JOBS=1 fo` pipeline once for
the final rebased commit. fo/fluff integration additionally runs a process-
boundary JSON oracle for success, diagnostics, fixes, and a forced child
failure. Update the contract documentation with every schema or behavior
change.
