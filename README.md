# fluff - A Modern Fortran Linter and Formatter

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Fortran](https://img.shields.io/badge/Fortran-2018-brightgreen.svg)](https://fortran-lang.org/)

**fluff** is a comprehensive linting and formatting tool for Fortran, inspired by Python's [ruff](https://github.com/astral-sh/ruff). It provides fast, reliable static analysis, automatic code formatting, and seamless integration with modern development workflows.

## ✨ Features

### 🚀 Core Functionality
- **15+ Style Rules (F001-F015)**: Enforce consistent Fortran style
- **7+ Performance Rules (P001-P007)**: Optimize code for better performance
- **Auto-fix Support**: Automatically fix many violations with `--fix`
- **Multiple Output Formats**: JSON, SARIF, XML, GitHub Actions annotations
- **Parallel Execution**: OpenMP-based parallel rule checking for speed
- **Advanced Formatting**: Intelligent line breaking at 88 characters with magic comment overrides

### 🛠️ Developer Experience
- **Language Server Protocol (LSP)**: Full IDE integration with hover, diagnostics, and code actions
- **Incremental Analysis**: Only re-analyze changed files
- **Smart Caching**: Intelligent caching system for faster subsequent runs
- **File Watching**: Automatic re-analysis on file changes
- **Configuration Options**: Control behavior via command-line arguments

### 🔌 Integrations
- **GitHub Actions**: Native support with annotations and problem matchers
- **Pre-commit Hooks**: Automatic linting in your git workflow
- **Editor Support**: VSCode, Vim, and Emacs plugins available
- **CI/CD Ready**: Proper exit codes and machine-readable output

## 📦 Installation

### Using fpm (Fortran Package Manager)
```bash
fpm install --profile release
```

### From Source
```bash
git clone https://github.com/yourusername/fluff.git
cd fluff
fpm build --profile release
```

## 🚀 Quick Start

### Basic Usage
```bash
# Check a single file
fluff check myfile.f90

# Check all Fortran files in a directory
fluff check src/

# Fix violations automatically
fluff check --fix src/

# Format code
fluff format src/
```

### Configuration
Configuration support is currently being developed. Command-line arguments can be used to control fluff behavior:

```bash
# Enable automatic fixing
fluff check --fix src/

# Show fix suggestions without applying
fluff check --show-fixes src/

# Set maximum line length
fluff format --line-length 100 src/

# Select output format
fluff check --output-format json src/
```

### Using Namelist Configuration (Alternative)
```fortran
&fluff_config
  fix = .true.
  show_fixes = .true.
  line_length = 100
  target_version = "2018"
  output_format = "json"
/
```

## 📋 Available Rules

### Style Rules (F-prefix)
- **F001**: Missing `implicit none` statement
- **F002**: Inconsistent indentation
- **F003**: Line too long
- **F004**: Trailing whitespace
- **F005**: Mixed tabs and spaces
- **F006**: Unused variable
- **F007**: Undefined variable
- **F008**: Missing intent declaration
- **F009**: Inconsistent intent usage
- **F010**: Obsolete Fortran features
- **F011**: Missing end block labels
- **F012**: Naming convention violations
- **F013**: Multiple statements per line
- **F014**: Unnecessary parentheses
- **F015**: Redundant continue statements

### Performance Rules (P-prefix)
- **P001**: Inefficient array operations
- **P002**: Poor loop ordering for cache
- **P003**: Array temporaries in expressions
- **P004**: Missing pure/elemental attributes
- **P005**: Inefficient string operations
- **P006**: Allocations inside loops
- **P007**: Mixed precision arithmetic

## 🔧 Advanced Features

### Formatting Magic Comments
Control formatter behavior with special comments:
```fortran
! Disable line breaking for specific sections
! fmt: skip
real :: very_long_variable_name_1, very_long_variable_name_2, very_long_variable_name_3
! fmt: on

! Alternative syntax
! fluff: noqa
real :: another_long_line_that_wont_be_broken
! fluff: qa
```

### Language Server Protocol (LSP)
```bash
# Start LSP server
fluff lsp

# Or configure your editor to start it automatically
```

### Output Formats

#### JSON Output
```bash
fluff check --output-format json src/ > report.json
```

#### SARIF (Static Analysis Results Interchange Format)
```bash
fluff check --output-format sarif src/ > report.sarif
```

#### GitHub Actions Annotations
```bash
fluff check --output-format github src/
```

### Pre-commit Integration
Add to `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: https://github.com/yourusername/fluff
    rev: v0.1.0
    hooks:
      - id: fluff
        args: [--fix]
```

## 🏗️ Architecture

fluff is built on top of the [fortfront](https://github.com/lazy-fortran/fortfront) AST library, providing:

- **AST-based Analysis**: Accurate semantic understanding of Fortran code
- **Type-aware Checks**: Leverages Hindley-Milner type inference
- **Control Flow Analysis**: Dead code and unreachable code detection
- **Dependency Graphs**: Module and file dependency tracking

## 🤝 Contributing

We welcome contributions! Please see our [Developer Guide](docs/DEVELOPER_GUIDE.md) for details on:

- Setting up a development environment
- Running tests
- Adding new rules
- Submitting pull requests

## 📊 Performance

fluff is designed for speed:
- Parallel rule execution with OpenMP
- Incremental analysis with smart caching
- Minimal memory footprint
- Processes large codebases in seconds

## 🐛 Troubleshooting

See our [Troubleshooting Guide](docs/TROUBLESHOOTING.md) for common issues and solutions.

## 📄 License

fluff is released under the MIT License. See [LICENSE](LICENSE) for details.

## 🙏 Acknowledgments

- Inspired by [ruff](https://github.com/astral-sh/ruff) for Python
- Built on [fortfront](https://github.com/lazy-fortran/fortfront) for AST parsing
- Uses [fpm](https://github.com/fortran-lang/fpm) for package management

## 📚 Documentation

- [API Reference](docs/API.md)
- [Developer Guide](docs/DEVELOPER_GUIDE.md)
- [Migration Guide](docs/MIGRATION.md)
- [Troubleshooting](docs/TROUBLESHOOTING.md)

---

**Note**: fluff is under active development. Some features may be experimental. Please report issues on our [GitHub tracker](https://github.com/yourusername/fluff/issues).