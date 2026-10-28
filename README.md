# fluff

fluff is an experimental linter and formatter for Fortran. It analyses source
through the [fortfront](https://github.com/lazy-fortran/fortfront) parser, AST
and semantic analysis instead of text matching.

Status: prototype under development; not yet ready for production use.

## Features

- Style rules `F001`-`F015`, performance rules `P001`-`P007` and correctness
  rule `C001`; `C002` and `C003` are available but off by default
  (`--select C002,C003`).
- `fluff check` with `--select`, `--ignore`, `--exclude`, `--fix`,
  `--show-fixes`, `--statistics` and stdin input via `-`.
- Text, JSON and SARIF diagnostics (`--output-format`).
- `fluff format` based on fortfront code emission, with `--check` and `--diff`.
- `fluff rules` to list rules, and an LSP server (`fluff server`).
- Configuration in a Fortran namelist file, `.fluff.nml` or `fluff.nml`.

## Install

Requires [fpm](https://fpm.fortran-lang.org) and a Fortran compiler (gfortran); fortfront is
fetched by fpm.

```bash
git clone https://github.com/lazy-fortran/fluff.git
cd fluff
fpm build --profile release
fpm install --profile release   # optional, installs to ~/.local/bin
```

## Usage

```bash
fluff check src/
fluff check --output-format json src/
fluff format --check src/
fluff format src/
```

`fluff --help` lists the options of the current build.

## Known limitations

- Part of the test suite cannot yet fail on a behavioural mismatch
  ([#262](https://github.com/lazy-fortran/fluff/issues/262)), so a green test
  run is weaker evidence than it should be.
- AST caching is disabled in the linter because fortfront arena copies are not
  yet safe to reuse.

## Documentation

- [docs/design.md](docs/design.md): architecture and the division of work with fo
- [ROADMAP.md](ROADMAP.md): planned work
- [CHANGELOG.md](CHANGELOG.md)

## Related projects

- [fortfront](https://github.com/lazy-fortran/fortfront): parser, AST, semantic
  analysis and code emission used by fluff.
- [fo](https://github.com/lazy-fortran/fo): build driver; `fo lint --deep` runs
  fluff and merges its findings.

## License

MIT, see [LICENSE](LICENSE).
