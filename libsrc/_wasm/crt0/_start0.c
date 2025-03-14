extern void __wasm_call_ctors(void);

void _start0(void) {
  __wasm_call_ctors();
}
