# Installation

fluff can be installed via prebuilt binaries, fpm, or from source.

## Prebuilt Binaries

Download the latest release from [GitHub Releases](https://github.com/lazy-fortran/fluff/releases).

### Linux x86_64

```bash
curl -LO https://github.com/lazy-fortran/fluff/releases/latest/download/fluff-linux-x86_64.tar.gz
tar -xzf fluff-linux-x86_64.tar.gz
sudo mv fluff /usr/local/bin/
```

### macOS ARM64 (Apple Silicon)

```bash
curl -LO https://github.com/lazy-fortran/fluff/releases/latest/download/fluff-macos-arm64.tar.gz
tar -xzf fluff-macos-arm64.tar.gz
sudo mv fluff /usr/local/bin/
```

### macOS x86_64 (Intel)

```bash
curl -LO https://github.com/lazy-fortran/fluff/releases/latest/download/fluff-macos-x86_64.tar.gz
tar -xzf fluff-macos-x86_64.tar.gz
sudo mv fluff /usr/local/bin/
```

## Using fpm (Fortran Package Manager)

If you have [fpm](https://fpm.fortran-lang.org/) installed:

```bash
fpm install fluff
```

This installs fluff to `~/.local/bin/` by default. Ensure this directory is in your PATH:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

## Building from Source

### Prerequisites

- gfortran 10+ or Intel Fortran 2021+
- fpm (Fortran Package Manager)

### Build Steps

```bash
git clone https://github.com/lazy-fortran/fluff.git
cd fluff
fpm build --profile release
```

The binary is located at `build/gfortran_*/fluff/app/fluff`.

To install system-wide:

```bash
fpm install --profile release
```

## Verifying Installation

After installation, verify fluff is working:

```bash
fluff --version
fluff --help
```

## Versioning

fluff follows [Semantic Versioning](https://semver.org/):

- Version format: `MAJOR.MINOR.PATCH`
- Pre-release versions use suffix: `v0.2.0-alpha.1`
- Release tags match version in `fpm.toml`

Current version is defined in `fpm.toml`.

## Troubleshooting

### Binary not found after installation

Ensure the installation directory is in your PATH:

```bash
# For fpm install
export PATH="$HOME/.local/bin:$PATH"

# For manual install to /usr/local/bin
export PATH="/usr/local/bin:$PATH"
```

### Permission denied on macOS

If macOS blocks the binary, remove the quarantine attribute:

```bash
xattr -d com.apple.quarantine /usr/local/bin/fluff
```

### Missing shared libraries

The release binaries are statically linked and should not require additional libraries. If you encounter issues, try building from source.

## Uninstalling

```bash
# If installed to /usr/local/bin
sudo rm /usr/local/bin/fluff

# If installed via fpm
rm ~/.local/bin/fluff
```
