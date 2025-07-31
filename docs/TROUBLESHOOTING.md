# Troubleshooting Guide

This document provides solutions to common issues when using fluff.

## Installation Issues

### `fluff` command not found
- Ensure fluff is installed correctly: `fpm install`
- Add the installation directory to your PATH
- Verify installation: `fluff --version`

### Permission denied when running fluff
- Make the binary executable: `chmod +x /path/to/fluff`
- Check file permissions and ownership

## Configuration Issues

### Configuration file not found
- Create `fluff.toml` in your project root
- Use `fluff init` to generate a default configuration
- Check configuration search order: current dir → parent dirs → home dir → global

### Invalid configuration syntax
- Validate TOML syntax using an online validator
- Check for missing quotes around strings
- Ensure proper section headers: `[tool.fluff]`

## Parsing Issues

### "Failed to parse Fortran file"
- Check file syntax with your Fortran compiler first
- Ensure file encoding is UTF-8
- Report parsing issues to the fortfront project

### "Unsupported Fortran features"
- fluff supports modern Fortran standards (2008+)
- Legacy features may not be fully supported
- Use `--ignore-unsupported` flag as workaround

## Performance Issues

### Slow analysis on large codebases
- Enable caching: `--cache-dir .fluff_cache`
- Use `--include` and `--exclude` patterns to limit scope
- Run analysis incrementally with `--diff-only`

### High memory usage
- Process files in smaller batches
- Increase system memory or use swap space
- Report memory issues with `--profile` flag

## Rule-Specific Issues

### Too many false positives
- Adjust rule configuration in `fluff.toml`
- Use `# fluff: ignore` comments for specific lines
- Configure per-file ignores for specific file patterns

### Missing expected violations
- Check if rules are enabled in configuration
- Verify rule severity levels
- Use `--verbose` flag for detailed output

## Integration Issues

### Editor integration not working
- Install the appropriate editor plugin
- Configure the fluff executable path
- Check editor error logs

### CI/CD pipeline failures
- Ensure fluff binary is available in CI environment
- Configure appropriate exit codes
- Use `--format=github` for GitHub Actions

## Getting Help

If you encounter issues not covered here:
1. Check the GitHub issues page
2. Enable verbose logging: `--log-level DEBUG`
3. Create a minimal reproduction case
4. Submit a bug report with full details