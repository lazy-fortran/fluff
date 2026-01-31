#!/bin/sh
# Non-interactive build artifact cleanup for CI and automated contexts.
# Removes the build/ directory without prompting (unlike fpm clean).

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$REPO_ROOT/build"

if [ -d "$BUILD_DIR" ]; then
    rm -rf "$BUILD_DIR"
    echo "Removed $BUILD_DIR"
else
    echo "Nothing to clean: $BUILD_DIR does not exist"
fi
