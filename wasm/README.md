WCC
===

C-compiler output .wasm binary.

[Online demo](https://tyfkda.github.io/xcc/)


### Requirements

  * C compiler (gcc or clang)
  * make
  * node.js (>=16), npm


### Build

Setup:

```sh
$ npm install
```

Build:

```sh
$ make
```

Generated files:

  * `wcc`: Compiler entry


### Usage

Compile:

```sh
$ ./wcc -o out.wasm foo.c
```

#### Command line options

  * `-o <filename>`: Set output filename (default: `a.wasm`)
  * `-I <path>`:     Add include path
  * `-D <label>(=value)`:  Define macro
  * `-entry-point=func_name`:  Specify entry point (default: _start)
  * `-e func_name,...`:  Export function names (comma separated)
  * `--stack-size=<size>`:  Set stack size (default: 8192)
  * `-nodefaultlibs`:  Ignore libc
  * `-nostdlib`:  Ignore libc and crt0
  * `--verbose`:  Output debug information


### Run

```sh
$ node runtime/runwasm.js out.wasm main foo bar baz
```
