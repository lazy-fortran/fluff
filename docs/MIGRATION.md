# Migration Guide

This guide helps you migrate to fluff from other Fortran linters and tools.

## Migration from Other Fortran Linters

### From fortls (Fortran Language Server)

fortls provides language server functionality but limited linting. To migrate:

1. **Install fluff**: `fpm install`
2. **Create configuration**: `fluff init` 
3. **Map fortls settings**: 
   - `fortls.lowercaseIntrinsics` → `fluff.rules.F012.lowercase-intrinsics`
   - `fortls.maxLineLength` → `fluff.line-length`

**Migration command:**
```bash
fluff migrate --from=fortls --config=.fortls
```

### From FORD (Fortran Documenter)

FORD focuses on documentation but has some style checks:

1. **Extract style rules** from FORD configuration
2. **Map to fluff rules**:
   - Line length limits → `F003`
   - Naming conventions → `F012`
3. **Import documentation requirements** → custom rules

### From Intel Fortran Compiler Warnings

Convert Intel compiler flags to fluff rules:

- `-warn all` → Enable all fluff rules
- `-warn nointerfaces` → Disable interface warnings
- `-stand f08` → Set Fortran standard to 2008

**Migration script:**
```bash
fluff migrate --from=ifort --compiler-flags="-warn all -stand f08"
```

## Migration from Generic Linters

### From pre-commit hooks

If using generic pre-commit hooks for Fortran:

1. **Replace generic hooks**:
   ```yaml
   # Before
   - repo: local
     hooks:
       - id: fortran-lint
         name: Fortran linting
         entry: bash -c 'find . -name "*.f90" | xargs custom_linter'
   
   # After  
   - repo: https://github.com/fortran-lang/fluff
     rev: v1.0.0
     hooks:
       - id: fluff
   ```

2. **Configure rule mapping**:
   - Line length checks → `F003`
   - Trailing whitespace → `F004`
   - Indentation → `F002`

### From EditorConfig

Map EditorConfig settings to fluff:

```ini
# .editorconfig
[*.f90]
indent_style = space
indent_size = 4
max_line_length = 88
trim_trailing_whitespace = true
```

**Equivalent fluff.toml:**
```toml
[tool.fluff.format]
indent-width = 4
max-line-length = 88

[tool.fluff.rules]
select = ["F002", "F003", "F004"]
```

## Configuration Migration Tools

### Automated Migration

fluff provides migration tools for common scenarios:

```bash
# Migrate from fortls
fluff migrate --from=fortls --input=.fortls --output=fluff.toml

# Migrate from pre-commit config  
fluff migrate --from=precommit --input=.pre-commit-config.yaml

# Migrate from EditorConfig
fluff migrate --from=editorconfig --input=.editorconfig
```

### Manual Migration Checklist

1. **□ Install fluff** and verify it works: `fluff --version`
2. **□ Create initial config**: `fluff init`
3. **□ Map old settings** to fluff configuration
4. **□ Test on sample files** to verify behavior
5. **□ Update CI/CD pipelines** to use fluff
6. **□ Update editor integration** 
7. **□ Train team** on new tool and workflow
8. **□ Remove old linting tools** and configurations

## Workflow Migration

### CI/CD Pipeline Updates

**GitHub Actions:**
```yaml
# Before (generic linter)
- name: Lint Fortran
  run: |
    find . -name "*.f90" -exec old_linter {} \;

# After (fluff)
- name: Lint with fluff  
  uses: fortran-lang/fluff-action@v1
  with:
    config: fluff.toml
```

**GitLab CI:**
```yaml
# Before
lint:fortran:
  script: old_linter src/*.f90

# After  
lint:fortran:
  script: fluff check src/
```

### Editor Integration Migration

**VS Code:**
1. Uninstall old Fortran extensions with linting
2. Install fluff VS Code extension
3. Configure workspace settings:
   ```json
   {
     "fluff.executable": "fluff",
     "fluff.configFile": "fluff.toml"
   }
   ```

**Vim/Neovim:**
```vim
" Replace old linter config
" Before: let g:fortran_linter = 'old_tool'
" After:
set makeprg=fluff\ check\ %
```

## Legacy Code Handling

### Gradual Migration Strategy

For large codebases with legacy code:

1. **Start with formatting rules** only:
   ```toml
   [tool.fluff.rules]
   select = ["F002", "F003", "F004", "F005"]  # Format only
   ```

2. **Add per-file ignores** for legacy files:
   ```toml
   [tool.fluff.per-file-ignores]
   "legacy/*.f90" = ["F010", "F011"]  # Allow obsolete features
   "vendor/*.f90" = ["ALL"]  # Skip vendor code entirely
   ```

3. **Gradually enable more rules** as code is modernized

### Incremental Adoption

Use fluff's incremental features:

```bash
# Only check changed files
fluff check --diff-only

# Check specific directories  
fluff check src/modern/ --ignore=src/legacy/

# Use different configs for different parts
fluff check src/new/ --config=strict.toml
fluff check src/legacy/ --config=relaxed.toml
```

## Common Migration Issues

### Issue: "Too many violations"
**Solution:** Start with basic formatting rules, gradually add more

### Issue: "False positives on legacy code" 
**Solution:** Use per-file ignores and legacy-specific configuration

### Issue: "Performance degradation"
**Solution:** Enable caching and use include/exclude patterns

### Issue: "Team resistance to new tool"
**Solution:** Provide training, show benefits, migrate incrementally

## Support and Resources

- **Migration Scripts**: Available in `tools/migrate/`
- **Example Configurations**: See `examples/migration/`
- **Community Support**: GitHub Discussions
- **Professional Support**: Available for enterprise migrations

For complex migrations, consider hiring a consultant familiar with both your existing setup and fluff.