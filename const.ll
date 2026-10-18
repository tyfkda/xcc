define i32 @sum(i8 %.param.n) {
	%V1 = alloca i8  ; (flag=40000808): n  : unsigned char, param(0)
	store i8 %.param.n, ptr %V1
	%V2 = alloca i64  ; (flag=40000000): acc  : long
	%V3 = alloca i32  ; (flag=40000000): i  : int

; BB 0
  br label %l.0001
l.0001: ; 
	store i64 0, ptr %V2
	store i32 1, ptr %V3
; BB 1
  br label %l.0006
l.0006: ;  from=[l.0001, l.0003]
	%1 = load i8, ptr %V1
	%2 = zext i8 %1 to i32
	%3 = load i32, ptr %V3
	%4 = icmp sgt i32 %3, %2
	br i1 %4, label %l.0004, label %l.0005
; BB 2
l.0005: ;  from=[l.0006]
	%5 = load i32, ptr %V3
	%6 = sext i32 %5 to i64
	%7 = load i64, ptr %V2
	%8 = add i64 %7, %6
	store i64 %8, ptr %V2
; BB 3
  br label %l.0003
l.0003: ;  from=[l.0005]
	%9 = load i32, ptr %V3
	%10 = add i32 %9, 1
	store i32 %10, ptr %V3
	br label %l.0006
; BB 4
l.0004: ;  from=[l.0006]
	%11 = load i64, ptr %V2
	%12 = trunc i64 %11 to i32
	ret i32 %12
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
	%13 = call i32 @sum(i8 10)
	ret i32 %13
; BB 1
}

