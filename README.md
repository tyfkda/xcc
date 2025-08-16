XCC
===

[![Action Status](https://github.com/tyfkda/xcc/workflows/AllTests/badge.svg)](https://github.com/tyfkda/xcc)
[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/tyfkda/xcc)

C compiler running on Linux or MacOS.

  * Supporting architecture: x86-64, aarch64 (arm64), riscv64, wasm
  * Binary format: ELF64


### Requirements

  * Linux (or MacOS)
  * C compiler (gcc or clang)
  * make


### Build

```sh
$ make
```

Generated files:

  * `xcc`: Compiler entry
  * `cpp`: Preprocessor
  * `cc1`: C compiler
  * `as`:  Assembler
  * `ld`:  Linker


### Usage

```sh
$ ./xcc -o hello examples/hello.c
$ ./hello
Hello, world!
```

#### Command line options

  * `-o <filename>`: Set output filename (default: `a.out`)
  * `-I <path>`:     Add include path
  * `-D <label>(=value)`:  Define macro
  * `-S`:            Output assembly code
  * `-E`:            Preprocess only
  * `-c`:            Output object file
  * `-nodefaultlibs`:  Ignore libc
  * `-nostdlib`:  Ignore libc and crt0


### TODO

  * Optimization
  * Archiver


### Reference

  * [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)
  * [rui314/9cc: A Small C Compiler](https://github.com/rui314/9cc)


----

### WebAssembly

Compile C to WebAssembly/WASI binary.

[Online demo](https://tyfkda.github.io/xcc/)

#### Requirements

  * node.js, npm
  * `llvm-ar`

#### Set up

```sh
$ npm ci
```

#### Build

```sh
$ make wcc
```

Generated files:

  * `wcc`: C compiler (including preprocessor, and output .wasm directly)

#### Usage

Compile:

```sh
$ ./wcc -o hello.wasm examples/hello.c
```

Command line options:

  * `-o <filename>`: Set output filename (default: `a.wasm`)
  * `-I <path>`:     Add include path
  * `-D <label>(=value)`:  Define macro
  * `-E`:            Preprocess only
  * `-c`:            Output object file
  * `--entry-point=func_name`:  Specify entry point (default: `_start`)
  * `-e func_name,...`:  Export function names (comma separated)
  * `--stack-size=<size>`:  Set stack size (default: 8192)
  * `-nodefaultlibs`:  Ignore libc
  * `-nostdlib`:  Ignore libc and crt0
  * `--verbose`:  Output debug information
  * `-Wl,...`:       Linker option
    * `--allow-undefined`:  Handles unresolved function symbols as imported on runtime
    * `--export-all`:  Export all non-static functions

#### Run

```sh
$ ./tool/runwasi hello.wasm
Hello, world!
```

You can also use WASM/WASI runtime (Wasmtime, Wasmer, etc.), too.

#### Missing features

  * `goto` statements are only allowed if
    they jump forward to a label right after a block,
    such as after a loop or `if` statement.
    Breaking from nested loops or blocks is allowed.
    Usage of `goto` not covered by the Linux kernel coding style,
    or outside the MISRA C:2023 Rule 15.3, fails to compile.
    
