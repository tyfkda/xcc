define i32 @main() {
	%V1 = alloca i32  ; (flag=40000000): acc  : int
	%V2 = alloca i32  ; (flag=40000000): i  : int

; BB 0
  br label %l.0002
l.0002: ; 
	store i32 0, ptr %V1
	store i32 1, ptr %V2
; BB 1
  br label %l.0007
l.0007: ;  from=[l.0002, l.0004]
	%1 = load i32, ptr %V2
	%2 = icmp sgt i32 %1, 5
	br i1 %2, label %l.0005, label %l.0006
; BB 2
l.0006: ;  from=[l.0007]
	%3 = load i32, ptr %V1
	%4 = load i32, ptr %V2
	%5 = add i32 %3, %4
	store i32 %5, ptr %V1
; BB 3
  br label %l.0004
l.0004: ;  from=[l.0006]
	%6 = load i32, ptr %V2
	%7 = add i32 %6, 1
	store i32 %7, ptr %V2
	br label %l.0007
; BB 4
l.0005: ;  from=[l.0007]
	%8 = load i32, ptr %V1
	ret i32 %8
; BB 5
}

