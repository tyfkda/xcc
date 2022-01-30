WCC
===

C-compiler output .wasm binary.

[Online demo](https://tyfkda.github.io/xcc/)


### Build

```sh
$ make
```

Generated files:

  * `wcc`: Compiler entry


### Usage

Compile:

```sh
$ ./wcc -oout.wasm foo.c
```

Run:

```sh
$ node runtime/runwasm.js out.wasm main foo bar baz
```


### Command line options

  * `-o<filename>`: Set output filename (default: a.wasm)
  * `-I<path>`:     Add include path
  * `-D<label>(=value)`:  Define macro
  * `-efunc_name,...`:  Export function names (comma separated)
  * `--stack-size=<size>`:  Set stack size (default: 8192)
  * `--verbose`:  Output debug information
