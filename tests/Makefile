CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition

SRC_DIR:=../src/cc

EXEDIR:=.
XCC:=../$(EXEDIR)/xcc
CPP:=../$(EXEDIR)/cpp

.PHONY: test-util test-cpp test-val test-sh

all:	unit-tests all-tests

unit-tests:	test-util

all-tests:	test-cpp test-val test-sh test-examples
	@echo 'All tests PASS!'

clean:
	rm -f util_test valtest a.out
test-util:	util_test
	@echo '## Util'
	@./util_test
	@echo ''

test-val:	valtest
	@echo '## valtest'
	@./valtest
	@echo ''

test-cpp: # $(CPP)
	@echo '## cpptest'
	CPP=$(CPP) ./cpptest.sh
	@echo ''

test-sh: # $(XCC)
	@echo '## test.sh'
	XCC=$(XCC) ./test.sh
	@echo ''

test-examples: # $(XCC)
	@echo '## Example test'
	XCC=$(XCC) ./example_test.sh
	@echo ''

UTIL_SRCS:=util_test.c $(SRC_DIR)/util.c
util_test:	$(UTIL_SRCS)
	gcc -o $@ $(CFLAGS) $^

VAL_SRCS:=../lib/crt0.c ../examples/util.c valtest.c
valtest:	$(VAL_SRCS) # $(XCC)
	$(XCC) -o$@ $(VAL_SRCS)
