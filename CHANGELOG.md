# Changelog

All notable changes to fluff will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-01-08

### Added
- Initial release of fluff - A Modern Fortran Linter and Formatter
- **22 Linting Rules**:
  - F001-F015: Style and formatting rules
  - P001-P007: Performance optimization rules
- **Auto-fix Support** for F001, F002, F008, P004
- **Multiple Output Formats**:
  - JSON with pretty printing
  - SARIF v2.1.0 for security tools
  - XML (generic, JUnit, CheckStyle)
  - GitHub Actions annotations
- **Language Server Protocol (LSP)**:
  - Full LSP server implementation
  - Hover information with type details
  - Code actions and quick fixes
  - Go to definition support
  - Real-time diagnostics
- **Performance Features**:
  - OpenMP-based parallel rule execution
  - Incremental analysis for changed files
  - Smart caching system
  - File watching with hot reload
- **Tool Integrations**:
  - GitHub Actions with annotations
  - Pre-commit hook support
  - Editor plugins (VSCode, Vim, Emacs)
  - CI/CD ready with proper exit codes
- **Advanced Analysis**:
  - Dead code detection
  - Dependency analysis with circular detection
  - Control flow analysis
  - Type inference via fortfront
- **Configuration**:
  - Namelist-based configuration
  - TOML configuration support
  - Per-file ignore patterns
  - Environment variable overrides
- **Developer Features**:
  - Comprehensive API for custom rules
  - Metrics and statistics collection
  - Multiple severity levels
  - Fix suggestion infrastructure

### Known Issues
- fortfront memory corruption in some complex scenarios (workarounds in place)
- Template error handling test failure (minor test design issue)

### Dependencies
- fortfront v0.1.0+ (Fortran AST library)
- Fortran stdlib
- json-fortran 8.3.0
- OpenMP 3.0+

### Contributors
- Christopher Albert (@krystophny)

---

## Future Releases

### [Planned for 0.2.0]
- Additional rule categories (C-rules for correctness, S-rules for security)
- Enhanced auto-fix coverage for all rules
- Improved fortfront integration with memory fixes
- Configuration profiles (strict, performance, legacy)
- Project-wide refactoring capabilities

### [Planned for 0.3.0]
- Cross-file analysis improvements
- Symbol renaming across projects
- Integration with popular build systems
- Performance profiling rules
- Custom rule plugin system