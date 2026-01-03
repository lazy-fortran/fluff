# Fluff Developer Guide

Welcome to the fluff development guide! This document provides comprehensive information for developers working with and contributing to fluff, the Fortran linter and formatter.

## Table of Contents

- [Development Setup](#development-setup)
- [Build System Integration](#build-system-integration)
- [CI/CD Integration](#cicd-integration)
- [Editor Integration](#editor-integration)
- [Contributing Guidelines](#contributing-guidelines)
- [Testing](#testing)
- [Performance Optimization](#performance-optimization)
- [Troubleshooting](#troubleshooting)

## Development Setup

### Prerequisites

- Fortran compiler (gfortran 9+ recommended)
- Fortran Package Manager (fpm)
- Git

### Quick Start

```bash
# Clone the repository
git clone https://github.com/fortran-lang/fluff.git
cd fluff

# Build the project
fpm build

# Run tests
fpm test

# Install locally
fpm install
```

### Development Build

For development with debug symbols:

```bash
fpm build --profile debug
```

For optimized release build:

```bash
fpm build --profile release
```

## Build System Integration

### FPM Integration

Fluff is built with fpm and integrates seamlessly with fpm-based projects:

```toml
# In your fpm.toml
[dependencies]
fluff = { git = "https://github.com/lazy-fortran/fluff" }

[[executable]]
name = "my_app"
source-dir = "app"
main = "main.f90"

# Add linting as a build step
[build]
auto-executables = true
```

### CMake Integration

For CMake projects, use the provided `FindFluff.cmake` module:

```cmake
# In your CMakeLists.txt
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
find_package(Fluff REQUIRED)

# Add fluff checking to a target
add_executable(my_app src/main.f90)
fluff_check_fortran(my_app)

# Add fluff formatting to a target
fluff_format_fortran(my_app)
```

Copy `scripts/cmake/FindFluff.cmake` to your project's cmake directory.

### Makefile Integration

For traditional Makefiles:

```makefile
# Variables
FLUFF = fluff
FORTRAN_SOURCES = $(wildcard src/*.f90)

# Targets
.PHONY: lint format check

lint: $(FORTRAN_SOURCES)
	$(FLUFF) check $^

format: $(FORTRAN_SOURCES)
	$(FLUFF) format --fix $^

check: lint
	@echo "All checks passed!"

# Add as dependency to your main target
all: check my_program

my_program: $(FORTRAN_SOURCES)
	gfortran -o $@ $^
```

## CI/CD Integration

### GitHub Actions

Use the provided GitHub Actions workflow (`.github/workflows/ci.yml`):

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - uses: fortran-lang/setup-fortran@v1
    - uses: fortran-lang/setup-fpm@v5
    
    - name: Build
      run: fpm build
      
    - name: Test
      run: fpm test
      
    - name: Lint
      run: fpm run fluff -- check src/
```

### GitLab CI

```yaml
# .gitlab-ci.yml
stages:
  - build
  - test
  - lint

variables:
  FPM_VERSION: "0.10.0"

before_script:
  - apt-get update -qq && apt-get install -y -qq gfortran wget
  - wget https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64
  - chmod +x fpm-${FPM_VERSION}-linux-x86_64
  - mv fpm-${FPM_VERSION}-linux-x86_64 /usr/local/bin/fpm

build:
  stage: build
  script:
    - fpm build

test:
  stage: test
  script:
    - fpm test

lint:
  stage: lint
  script:
    - fpm run fluff -- check src/
  allow_failure: true
```

### Pre-commit Hooks

Install the pre-commit hook:

```bash
# Copy the pre-commit hook
cp scripts/hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

# Configure options (optional)
git config fluff.autofix true
git config fluff.failonwarnings false
```

The pre-commit hook supports several options:

```bash
# Run with auto-fix
.git/hooks/pre-commit --auto-fix

# Fail on warnings
.git/hooks/pre-commit --fail-on-warnings

# Check all files (not just staged)
.git/hooks/pre-commit --check-all

# Use custom options
.git/hooks/pre-commit --fix --line-length 100
```

## Editor Integration

### Visual Studio Code

Install the fluff extension from the marketplace or build from source:

```bash
cd editors/vscode/fluff-fortran
npm install
npm run compile
```

Features:
- Syntax highlighting for Fortran
- Real-time linting with problem highlights
- Format document/selection
- Code actions for fixes
- Configuration through VS Code settings

Configuration options:

```json
{
  "fluff.enable": true,
  "fluff.executable": "fluff",
  "fluff.configFile": "(not yet supported)",
  "fluff.linting.enabled": true,
  "fluff.linting.onSave": true,
  "fluff.formatting.enabled": true,
  "fluff.formatting.onSave": false,
  "fluff.autoFix": false
}
```

### Vim/Neovim

For vim integration, add to your `.vimrc`:

```vim
" Fluff integration
autocmd FileType fortran setlocal makeprg=fluff\ check\ %
autocmd FileType fortran setlocal errorformat=%f:%l:%c:\ %t%*[^:]:\ %m

" Format with fluff
command! FluffFormat !fluff format --fix %
command! FluffCheck !fluff check %

" Key mappings
nnoremap <leader>lf :FluffFormat<CR>
nnoremap <leader>lc :FluffCheck<CR>
```

### Emacs

Add to your Emacs configuration:

```elisp
;; Fluff integration
(defun fluff-check ()
  "Run fluff check on current buffer"
  (interactive)
  (compile (concat "fluff check " (buffer-file-name))))

(defun fluff-format ()
  "Run fluff format on current buffer"
  (interactive)
  (shell-command (concat "fluff format --fix " (buffer-file-name)))
  (revert-buffer nil t))

;; Key bindings for Fortran mode
(add-hook 'fortran-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c l c") 'fluff-check)
    (local-set-key (kbd "C-c l f") 'fluff-format)))
```

## Contributing Guidelines

### Code Style

- Follow existing code conventions
- Use 4 spaces for indentation
- Maximum line length: 88 characters
- Write self-documenting code
- Add tests for new features

### Development Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Write tests first (TDD approach)
4. Implement the feature
5. Run tests: `fpm test`
6. Run linting: `fpm run fluff -- check src/`
7. Commit changes with descriptive messages
8. Push to your fork
9. Create a pull request

### Commit Message Format

```
Type: Brief description (50 chars max)

Detailed explanation of what and why, not how.
Wrap at 72 characters.

- Bullet points are okay
- Use present tense: "Add feature" not "Added feature"
- Reference issues: "Fixes #123"
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

## Testing

### Test Structure

Tests are organized by functionality:

```
test/
├── test_core.f90              # Core functionality
├── test_linter.f90            # Linting rules
├── test_formatter.f90         # Code formatting
├── test_config.f90            # Configuration
└── integration/               # Integration tests
```

### Running Tests

```bash
# All tests
fpm test

# Specific test
fpm test test_core

# Verbose output
fpm test --verbose

# Debug build tests
fpm test --profile debug
```

### Writing Tests

Follow the existing test patterns:

```fortran
program test_my_feature
    use fluff_core
    implicit none
    
    integer :: total_tests = 0
    integer :: passed_tests = 0
    
    call test_basic_functionality()
    call test_edge_cases()
    call test_error_handling()
    
    call print_test_summary(total_tests, passed_tests)
    
contains
    
    subroutine test_basic_functionality()
        ! Test implementation
        call assert_equals(expected, actual, "Basic functionality")
    end subroutine
    
end program test_my_feature
```

### Performance Testing

Benchmark your changes:

```bash
# Build optimized version
fpm build --profile release

# Run performance tests
time fpm run fluff -- check large_project/

# Compare with baseline
scripts/benchmark.sh
```

## Performance Optimization

### Profiling

Use gfortran's profiling capabilities:

```bash
# Build with profiling
fpm build --flag "-pg"

# Run and generate profile
fpm run fluff -- check src/
gprof build/gfortran_*/app/fluff gmon.out > profile.txt
```

### Memory Usage

Monitor memory usage:

```bash
# Use valgrind for memory analysis
valgrind --tool=memcheck fpm run fluff -- check src/

# Monitor with time
/usr/bin/time -v fpm run fluff -- check src/
```

### Optimization Guidelines

1. **Avoid unnecessary allocations** in hot paths
2. **Cache frequently accessed data**
3. **Use appropriate data structures**
4. **Minimize string operations**
5. **Profile before optimizing**

## Troubleshooting

### Common Issues

#### Build Failures

**Problem**: Compilation errors
```bash
# Check compiler version
gfortran --version

# Clean and rebuild
fpm clean
fpm build
```

**Problem**: Missing dependencies
```bash
# Update fpm
fpm update

# Check fpm.toml dependencies
cat fpm.toml
```

#### Runtime Errors

**Problem**: Segmentation fault
```bash
# Build with debug info
fpm build --profile debug

# Run with debugger
gdb build/gfortran_*/app/fluff
```

**Problem**: Configuration options
```bash
# Use command-line arguments
fluff check --fix --line-length 100 src/

# Configuration files are not yet supported
# Use CLI options instead
```

### Performance Issues

**Problem**: Slow linting
- Profile the code to identify bottlenecks
- Check if AST caching is enabled
- Reduce the number of active rules

**Problem**: High memory usage
- Enable incremental analysis
- Reduce cache size limits
- Check for memory leaks with valgrind

### Getting Help

1. Check the [FAQ](FAQ.md)
2. Search existing [GitHub issues](https://github.com/fortran-lang/fluff/issues)
3. Create a new issue with:
   - Fluff version (`fluff --version`)
   - System information
   - Minimal reproduction case
   - Expected vs actual behavior

## Development Tools

### Useful Scripts

```bash
# Development scripts
scripts/
├── build.sh                   # Build script
├── test.sh                    # Test runner
├── benchmark.sh               # Performance benchmarks
├── format.sh                  # Format codebase
└── release.sh                 # Release preparation
```

### IDE Configuration

Recommended VS Code extensions:
- Modern Fortran
- GitLens
- Fortran IntelliSense
- Test Explorer

Settings for Fortran development:

```json
{
  "fortran.formatting.formatter": "fluff",
  "fortran.linting.compiler": "gfortran",
  "files.associations": {
    "*.f90": "fortran-modern"
  }
}
```

## Architecture Overview

### Core Components

```
src/
├── fluff_core.f90             # Core types and utilities
├── fluff_ast.f90              # AST manipulation
├── fluff_linter.f90           # Linting engine
├── fluff_formatter.f90        # Formatting engine
├── fluff_cli.f90              # Command-line interface
├── fluff_config.f90           # Configuration management
├── fluff_rules/               # Built-in rules
├── fluff_diagnostics.f90      # Error reporting
└── fluff_server.f90           # LSP server
```

### Data Flow

1. **Input**: Source files or stdin
2. **Parse**: Create AST using fortfront
3. **Analyze**: Apply linting rules
4. **Format**: Apply formatting rules
5. **Output**: Diagnostics or formatted code

## Release Process

1. Update version in `fpm.toml`
2. Update `CHANGELOG.md`
3. Run full test suite
4. Create release branch
5. Tag release: `git tag v1.0.0`
6. Push tags: `git push --tags`
7. GitHub Actions handles the release build

---

For more information, see the [API documentation](API.md) and [rule development guide](RULES.md).
