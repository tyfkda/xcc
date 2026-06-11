; コンパイルして実行：
; $ llc examples/llvm_test.ll && gcc examples/llvm_test.s && ./a.out; echo $?

define i32 @main(i32, i8**) {
  %3 = add i32 1, 2
  ret i32 %3
}
