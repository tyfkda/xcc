
.PHONY: test-util test-sh

all:	../xcc test-util test-sh

../xcc:
	make -C .. xcc

test-util:	util_test
	./util_test

test-sh:	test.sh
	./test.sh

util_test:	util_test.c ../util.c
	gcc -o $@ $^
