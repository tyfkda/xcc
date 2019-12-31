SRC_DIR:=../src/cc
UTIL_DIR:=../src/util

CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition
CFLAGS+=-I$(SRC_DIR) -I$(UTIL_DIR)

EXEDIR:=.
XCC:=../$(EXEDIR)/xcc
CPP:=../$(EXEDIR)/cpp

.PHONY: all
all:	test

.PHONY: test
test:	unit-tests cpp-tests cc-tests
	@echo 'All tests PASS!'

.PHONY: unit-tests
unit-tests:	test-util

.PHONY: cpp-tests
cpp-tests:	test-cpp

.PHONY: cc-tests
cc-tests:	test-sh test-val test-examples

.PHONY: clean
clean:
	rm -f util_test valtest a.out tmp.s

.PHONY: test-util
test-util:	util_test
	@echo '## Util'
	@./util_test
	@echo ''

.PHONY: test-val
test-val:	valtest
	@echo '## valtest'
	@./valtest
	@echo ''

.PHONY: test-cpp
test-cpp: # $(CPP)
	@echo '## cpptest'
	CPP=$(CPP) ./cpptest.sh
	@echo ''

.PHONY: test-sh
test-sh: # $(XCC)
	@echo '## test.sh'
	XCC=$(XCC) ./test.sh
	rm -f core
	@echo ''

.PHONY: test-examples
test-examples: # $(XCC)
	@echo '## Example test'
	XCC=$(XCC) ./example_test.sh
	@echo ''

UTIL_SRCS:=util_test.c $(UTIL_DIR)/util.c
util_test:	$(UTIL_SRCS)
	gcc -o $@ $(CFLAGS) $^

VAL_SRCS:=../lib/crt0.c ../examples/util.c valtest.c
valtest:	$(VAL_SRCS) # $(XCC)
	$(XCC) -o$@ $(VAL_SRCS)
