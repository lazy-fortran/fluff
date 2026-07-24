# fluff

`fluff` is an experimental Fortran linter and formatter built on the
FortFront AST and semantic APIs.

The project is useful as a FortFront-based tool prototype, but it is not yet a
Ruff-equivalent production tool. Several CLI and formatter features are still
tracked as open issues.

## Role in the toolchain

`fluff` is the deep static-analysis half of a two-tool split, in the shape Go
uses for `go vet` and staticcheck, Rust uses for cargo and clippy, and C and
C++ use for compiler warnings and clang-tidy.

`fo` owns the cheap tier: checks that need no parse tree, run on every
invocation, and work with nothing else installed. Today that is unused imports,
short-circuit reliance, and gfortran's own warnings.

`fluff` owns every rule that needs an abstract syntax tree, because it is built
on FortFront and `fo` is not. Type-aware rules, dead-code analysis, and
column-major access patterns belong here. `fo lint --deep` reaches them by
running `fluff check --output-format json` as a subprocess and merging the
findings (lazy-fortran/fo#59).

Two consequences worth stating explicitly:

- Do not reimplement `fo`'s native rules here. They must keep working when
  `fluff` is not installed.
- The subprocess boundary is deliberate. It keeps `fo`'s dependency closure
  free of FortFront, so `fo build` and `fo test` stay available during
  bootstrap.

Before `fo lint --deep` can rely on this repository, #260, #261, #262, and #263
need to close. Take #262 first: while 28 test programs exit zero regardless of
what they find, a passing run here is not evidence that the other three were
fixed.

## Current Scope

Implemented or partially implemented:

- style rules `F001` to `F015`
- performance rules `P001` to `P007`
- correctness rule `C001`
- formatter built on FortFront `emit_fortran`
- basic LSP components
- JSON/SARIF/GitHub-style output code paths
- CLI: `--select`, `--ignore`, `--exclude`, `--statistics`, `--quiet`,
  `--show-fixes`, the `rules` listing, and stdin input via `-`

Known limits:

- `fluff format` is not idempotent: it prepends a space to the first line and
  writes `print * , i` for `print *, i` (#260)
- `F006` reports an array as unused when it is only read through a subscript
  (#261)
- the test suite cannot detect either of the above, because 28 of its 94
  programs exit zero no matter what they find (#262), and two more resolve the
  binary under test by globbing fpm's build layout, so they can exercise a
  stale artifact instead of the current tree (#265)
- formatter can still move inline comments in unsafe ways (#244)
- configuration support has open regressions
- AST caching is disabled in the linter path because FortFront arena/context
  copies are not yet safe enough
  (`src/fluff_linter/fluff_linter.f90:83`), which leaves
  `fluff_analysis_cache.f90` compiled but unreachable

## Install

```bash
git clone git@github.com:lazy-fortran/fluff.git
cd fluff
fpm build --profile release
```

## Basic Usage

```bash
fluff check src/
fluff format src/
fluff check --output-format json src/
```

Use `fluff --help` for the options supported by the current build.

## Architecture

`fluff` should not parse Fortran text itself. The intended flow is:

1. read source
2. parse with FortFront tooling APIs
3. run FortFront semantic analysis
4. execute AST-based lint rules
5. format through FortFront code emission plus local cleanup passes

This keeps parser and semantic behavior aligned with FortFront and avoids
regex-based language analysis.

## Roadmap

See [ROADMAP.md](ROADMAP.md) and the open issues. The short version:

1. fix formatter safety first
2. finish CLI parity needed for daily use
3. repair configuration loading
4. only then improve cache/LSP polish

## Related Projects

- [fortfront](https://github.com/lazy-fortran/fortfront): parser, AST,
  semantic analysis, formatter emission.
- [standard](https://github.com/lazy-fortran/standard): target language-mode
  behavior for LFortran Standard and Infer.

## License

MIT. See `LICENSE`.
