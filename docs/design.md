# fluff design

## Analysis pipeline

fluff does not parse Fortran text itself. Every file goes through the same
steps:

1. read the source;
2. parse it with the fortfront tooling API;
3. run fortfront semantic analysis;
4. run the AST-based lint rules;
5. for formatting, emit code through fortfront and apply local clean-up passes.

Keeping parsing and semantic analysis in fortfront means that fluff sees the
same language as the rest of the lazy-fortran tools, and avoids regex-based
analysis.

## Relation to fo

Static checks for Fortran are split between two tools, similar to `go vet` and
staticcheck in Go, or compiler warnings and clang-tidy in C and C++.

[fo](https://github.com/lazy-fortran/fo) runs the inexpensive checks that need no
syntax tree: unused imports, reliance on short-circuit evaluation, and the
compiler's own warnings. They run on every invocation and need nothing else
installed.

fluff owns the rules that need an AST, such as type-aware checks, dead-code
analysis and array access patterns. `fo lint --deep` runs
`fluff check --output-format json` as a subprocess and merges the findings
with its own.

Two consequences follow:

- fo's native rules are not reimplemented in fluff; they must keep working when
  fluff is not installed.
- The subprocess boundary is deliberate. It keeps fortfront out of fo's
  dependencies, so `fo build` and `fo test` remain usable while the rest of the
  toolchain is being bootstrapped.

## Output

JSON diagnostics keep complete messages and escape quotes, backslashes and
control characters. Explicit file lists may contain more than 100 paths and
file names longer than 256 characters; command-line arguments are read at their
actual length.

A more detailed, partly aspirational description of the rule system, formatter
and LSP server is in [DESIGN.md](../DESIGN.md).
