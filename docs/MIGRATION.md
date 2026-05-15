# Migration Notes

`fluff` is not ready to replace a production lint/format stack wholesale.
Adopt it gradually and keep existing CI checks until the relevant rules and
formatter behavior have been validated on your codebase.

## Recommended Adoption

1. Build `fluff` from source.
2. Run `fluff check` on a small source subset.
3. Review diagnostics manually and decide which rules are reliable enough for
   your code.
4. Run `fluff format` only on files where semantic preservation has been
   checked.
5. Add CI after rule selection, config loading, and output format are stable for
   your workflow.

## Current Migration Limits

- Automated migration commands from `fortls`, EditorConfig, pre-commit, or
  compiler flags are not implemented.
- Per-file ignores and full Ruff-style rule selection are still roadmap items.
- Formatter output must be reviewed carefully while inline-comment preservation
  remains open.

## Practical CI Pattern

Use the narrowest command that works for the current build:

```bash
fluff check src/
```

Do not depend on unimplemented migration subcommands or advertised Ruff parity.
