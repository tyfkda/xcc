CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

xcc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): xcc.h

test:	xcc
	make -C tests

clean:
	rm -f xcc *.o *~ tmp*
	make -C tests clean

codegen.o: codegen.c x86_64.h xcc.h
main.o: main.c x86_64.h xcc.h
