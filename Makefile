CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

xcc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): xcc.h

test: xcc
	./xcc -test
	./test.sh

clean:
	rm -f xcc *.o *~ tmp*

codegen.o: codegen.c x86_64.h xcc.h
main.o: main.c x86_64.h xcc.h
