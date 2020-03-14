XCC
===

[![Build Status](https://travis-ci.org/tyfkda/xcc.svg?branch=master)](https://travis-ci.org/tyfkda/xcc)
[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/tyfkda/xcc)

  * C compiler for [XV6 (64bit)](https://github.com/tyfkda/xv6)
    * Also work on Linux
  * Assembler
  * Output ELF file format directly
  * Register allocation (Linear scan)
  * Self hosting


### Requirements

  * Linux
  * gcc or clang
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


### Run

```sh
$ ./xcc examples/hello.c
$ ./a.out
```


### Commandline options

  * `-o<filename>`: Set output filename (default: a.out)
  * `-I<path>`:     Add include path
  * `-D<label>(=value)`:  Define macro
  * `-S`:           Output source code (, not execlutable) to stdout
  * `-E`:           Preprocess only
  * `--dump-ir`:    Output IR code to stdout (debug purpose)


### TODO

  * Optimization
  * Archiver, Linker


### Missing features

Compiler:

  * floating point numbers
  * Bit field

Preprocessor:

  * Self recursive macro expansion


### Reference

  * [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)
  * [rui314/9cc: A Small C Compiler](https://github.com/rui314/9cc)
