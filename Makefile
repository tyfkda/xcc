CFLAGS=-Wall -Werror -std=c11
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
