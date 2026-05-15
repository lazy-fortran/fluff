# fluff

`fluff` is an experimental Fortran linter and formatter built on the
FortFront AST and semantic APIs.

The project is useful as a FortFront-based tool prototype, but it is not yet a
Ruff-equivalent production tool. Several CLI and formatter features are still
tracked as open issues.

## Current Scope

Implemented or partially implemented:

- style rules `F001` to `F015`
- performance rules `P001` to `P007`
- correctness rule `C001`
- formatter built on FortFront `emit_fortran`
- basic LSP components
- JSON/SARIF/GitHub-style output code paths

Known limits:

- configuration support has open regressions
- formatter can still move inline comments in unsafe ways
- rule selection/ignore CLI parity with Ruff is incomplete
- stdin, quiet mode, statistics, rule listing, and show-fixes UX are tracked as
  open issues
- AST caching is disabled in the linter path because FortFront arena/context
  copies are not yet safe enough

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
