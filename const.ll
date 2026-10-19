@x = global i32 100

define i32 @sum(i8 %.param.n) {
	%V1 = alloca i8  ; (flag=40000808): n  : unsigned char, param(0)
	store i8 %.param.n, ptr %V1
	%V2 = alloca i64  ; (flag=40000000): acc  : long
	%V3 = alloca i32  ; (flag=40000000): i  : int

; BB 0
  br label %l.0002
l.0002: ; 
	store i64 0, ptr %V2
	store i32 1, ptr %V3
; BB 1
  br label %l.0007
l.0007: ;  from=[l.0002, l.0004]
	%1 = load i8, ptr %V1
	%2 = zext i8 %1 to i32
	%3 = load i32, ptr %V3
	%4 = icmp sgt i32 %3, %2
	br i1 %4, label %l.0005, label %l.0006
; BB 2
l.0006: ;  from=[l.0007]
	%5 = load i32, ptr %V3
	%6 = sub i32 %5, 1
	%7 = bitcast i32 %6 to i32
	%8 = icmp sgt i32 %7, 4
	br i1 %8, label %l.0015, label %l.0016
; BB 3
l.0016: ;  from=[l.0006]
	switch i32 %7, label %.tjmp.9 [
		i32 0,label %l.0010
		i32 1,label %l.0011
		i32 2,label %l.0012
		i32 3,label %l.0013
		i32 4,label %l.0014
	]
.tjmp.9:	unreachable
; BB 4
l.0010: ;  from=[l.0016]
	%10 = load i64, ptr %V2
	%11 = add i64 %10, 11
	store i64 %11, ptr %V2
	br label %l.0004
; BB 5
l.0011: ;  from=[l.0016]
	%12 = load i64, ptr %V2
	%13 = add i64 %12, 22
	store i64 %13, ptr %V2
	br label %l.0004
; BB 6
l.0012: ;  from=[l.0016]
	%14 = load i64, ptr %V2
	%15 = add i64 %14, 33
	store i64 %15, ptr %V2
	br label %l.0004
; BB 7
l.0013: ;  from=[l.0016]
	%16 = load i64, ptr %V2
	%17 = add i64 %16, 44
	store i64 %17, ptr %V2
	br label %l.0004
; BB 8
l.0014: ;  from=[l.0016]
	%18 = load i64, ptr %V2
	%19 = add i64 %18, 55
	store i64 %19, ptr %V2
	br label %l.0004
; BB 9
l.0015: ;  from=[l.0006]
	%20 = load i32, ptr %V3
	%21 = sext i32 %20 to i64
	%22 = load i64, ptr %V2
	%23 = add i64 %22, %21
	store i64 %23, ptr %V2
; BB 10
  br label %l.0004
l.0004: ;  from=[l.0014, l.0013, l.0012, l.0011, l.0010, l.0015]
	%24 = load i32, ptr %V3
	%25 = add i32 %24, 1
	store i32 %25, ptr %V3
	br label %l.0007
; BB 11
l.0005: ;  from=[l.0007]
	%26 = load i64, ptr %V2
	%27 = trunc i64 %26 to i32
	ret i32 %27
; BB 12
}


@l.0001 = internal global i32 10

define i32 @main(i32 %.param.argc, i64 %.param.argv) {
	%V1 = alloca i32  ; (flag=40000008): argc  : int, param(0)
	store i32 %.param.argc, ptr %V1
	%V2 = alloca i64  ; (flag=40000808): argv  : char**, param(1)
	store i64 %.param.argv, ptr %V2

; BB 0
  br label %l.0025
l.0025: ; 
	%28 = getelementptr i8, ptr @l.0001
	%29 = load i32, ptr %28
	%30 = trunc i32 %29 to i8
	; pusharg %31 (index=#0)
	%31 = call i32 @sum(i8 %30)
	%32 = getelementptr i8, ptr @x
	%33 = load i32, ptr %32
	%34 = icmp sgt i32 %33, 0
	%35 = zext i1 %34 to i8
	%36 = sext i8 %35 to i32
	%37 = add i32 %31, %36
	ret i32 %37
; BB 1
}

