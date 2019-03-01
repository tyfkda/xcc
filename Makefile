CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition
SRCS=util.c lexer.c parser.c codegen.c elfutil.c main.c
OBJS=$(SRCS:.c=.o)
CPPOBJS=util.o cpp.o

xcc: $(OBJS) cpp
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

cpp: $(CPPOBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

$(OBJS): xcc.h

test:	xcc
	make -C tests

clean:
	rm -f xcc *.o *~ tmp*
	make -C tests clean

codegen.o: codegen.c x86_64.h xcc.h
main.o: main.c x86_64.h xcc.h
