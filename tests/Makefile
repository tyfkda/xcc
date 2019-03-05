CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition

.PHONY: test-util test-val test-sh

all:	../xcc test-util test-val test-sh test-examples
	@echo 'All tests PASS!'

clean:
	rm -f util_test valtest a.out

../xcc:
	make -C .. xcc

test-util:	util_test
	@echo '## Util'
	@./util_test
	@echo ''

test-val:	valtest
	@echo '## valtest'
	@./valtest
	@echo ''

test-sh:	test.sh
	@echo '## test.sh'
	@./test.sh
	@echo ''

test-examples:	example_test.sh
	@echo '## Example test'
	@./example_test.sh
	@echo ''

util_test:	util_test.c ../util.c
	gcc -o $@ $(CFLAGS) $^

valtest:	../examples/util.c valtest.c ../xcc
	../xcc -o valtest ../examples/util.c valtest.c
