
define i32 @main(i32 %.param.argc, ptr %.param.argv) {
	%V1 = alloca i32  ; (flag=40000008): argc  : int, param(0)
	store i32 %.param.argc, ptr %V1
	%V2 = alloca ptr  ; (flag=40001808): argv  : char**, param(1)
	store ptr %.param.argv, ptr %V2
	%V3 = alloca i32  ; (flag=40000000): acc  : int
	%V4 = alloca i32  ; (flag=40000000): i  : int
	%V5 = alloca i32  ; (flag=40000000): i  : int
	%Va = alloca [5 x i32], align 4  ; int[5], stack-offset=   0, storage=0x200

; BB 0
  br label %l.0001
l.0001: ; 
	store i32 0, ptr %V4
; BB 1
  br label %l.0006
l.0006: ;  from=[l.0001, l.0003]
	%1 = load i32, ptr %V4
	%2 = icmp sge i32 %1, 5
	br i1 %2, label %l.0004, label %l.0005
; BB 2
l.0005: ;  from=[l.0006]
	%3 = load i32, ptr %V4
	%4 = load i32, ptr %V4
	%5 = mul i32 %3, %4
	%6 = getelementptr ptr, ptr %Va
	%7 = load i32, ptr %V4
	%8 = sext i32 %7 to i64
	%9 = shl i64 %8, 2
	%10 = getelementptr ptr, ptr %6, i64 %9
	store i32 %5, ptr %10
; BB 3
  br label %l.0003
l.0003: ;  from=[l.0005]
	%11 = load i32, ptr %V4
	%12 = add i32 %11, 1
	store i32 %12, ptr %V4
	br label %l.0006
; BB 4
l.0004: ;  from=[l.0006]
	store i32 0, ptr %V3
	store i32 0, ptr %V5
; BB 5
  br label %l.0011
l.0011: ;  from=[l.0004, l.0008]
	%13 = load i32, ptr %V5
	%14 = icmp sge i32 %13, 5
	br i1 %14, label %l.0009, label %l.0010
; BB 6
l.0010: ;  from=[l.0011]
	%15 = getelementptr ptr, ptr %Va
	%16 = load i32, ptr %V5
	%17 = sext i32 %16 to i64
	%18 = shl i64 %17, 2
	%19 = getelementptr ptr, ptr %15, i64 %18
	%20 = load i32, ptr %19
	%21 = load i32, ptr %V3
	%22 = add i32 %21, %20
	store i32 %22, ptr %V3
; BB 7
  br label %l.0008
l.0008: ;  from=[l.0010]
	%23 = load i32, ptr %V5
	%24 = add i32 %23, 1
	store i32 %24, ptr %V5
	br label %l.0011
; BB 8
l.0009: ;  from=[l.0011]
	%25 = load i32, ptr %V3
	ret i32 %25
; BB 9
}

