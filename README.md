# elan-cleanup

This script deletes unused toolchains from your `~/.elan` directory to free up
some space. It detects used toolchains by looking for `lean-toolchain` files
in your home directory (and subdirectories, excluding directories starting with
`.`).

## Building

With `elan` installed, run `lake build`.

## Usage

After building, run `build/bin/elan-cleanup`. You can also copy this binary to
a directory in your `PATH`.
