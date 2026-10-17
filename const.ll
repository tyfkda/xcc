define i32 @sum(i32 %.param.n) {
	%V1 = alloca i32  ; (flag=40000008): n  : int, param(0)
	store i32 %.param.n, ptr %V1
	%V2 = alloca i32  ; (flag=40000000): acc  : int
	%V3 = alloca i32  ; (flag=40000000): i  : int

; BB 0
  br label %l.0001
l.0001: ; 
	store i32 0, ptr %V2
	store i32 1, ptr %V3
; BB 1
  br label %l.0006
l.0006: ;  from=[l.0001, l.0003]
	%1 = load i32, ptr %V3
	%2 = load i32, ptr %V1
	%3 = icmp sgt i32 %1, %2
	br i1 %3, label %l.0004, label %l.0005
; BB 2
l.0005: ;  from=[l.0006]
	%4 = load i32, ptr %V2
	%5 = load i32, ptr %V3
	%6 = add i32 %4, %5
	store i32 %6, ptr %V2
; BB 3
  br label %l.0003
l.0003: ;  from=[l.0005]
	%7 = load i32, ptr %V3
	%8 = add i32 %7, 1
	store i32 %8, ptr %V3
	br label %l.0006
; BB 4
l.0004: ;  from=[l.0006]
	%9 = load i32, ptr %V2
	ret i32 %9
; BB 5
}

define i32 @main(i32 %.param.argc, i64 %.param.argv) {
	%V1 = alloca i32  ; (flag=40000008): argc  : int, param(0)
	store i32 %.param.argc, ptr %V1
	%V2 = alloca i64  ; (flag=40000808): argv  : char**, param(1)
	store i64 %.param.argv, ptr %V2

; BB 0
  br label %l.0009
l.0009: ; 
	; pusharg 10 (index=#0)
	%10 = call i32 @sum(i32 10)
	ret i32 %10
; BB 1
}

