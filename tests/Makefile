SRC_DIR:=../src/cc
UTIL_DIR:=../src/util

CFLAGS=-ansi -std=c11 -Wall -Wextra -Werror \
	-Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition \
	-Wno-empty-body
CFLAGS+=-I$(SRC_DIR) -I$(UTIL_DIR)

EXE_DIR:=.
XCC:=../$(EXE_DIR)/xcc
CPP:=../$(EXE_DIR)/cpp

.PHONY: all
all:	test

.PHONY: test
test:	unit-tests cpp-tests cc-tests
	@echo 'All tests PASS!'

.PHONY: unit-tests
unit-tests:	test-table test-util test-parser

.PHONY: cpp-tests
cpp-tests:	test-cpp

.PHONY: cc-tests
cc-tests:	test-sh test-val test-examples

.PHONY: clean
clean:
	rm -f table_test util_test parser_test valtest a.out tmp.s

.PHONY: test-table
test-table:	table_test
	@echo '## NameTable'
	@./table_test
	@echo ''

.PHONY: test-util
test-util:	util_test
	@echo '## Util'
	@./util_test
	@echo ''

.PHONY: test-parser
test-parser:	parser_test
	@echo '## Parser'
	@./parser_test
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

TABLE_SRCS:=table_test.c $(UTIL_DIR)/table.c
table_test:	$(TABLE_SRCS)
	gcc -o $@ $(CFLAGS) $^

UTIL_SRCS:=util_test.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
util_test:	$(UTIL_SRCS)
	gcc -o $@ $(CFLAGS) $^

PARSER_SRCS:=parser_test.c $(SRC_DIR)/parser_expr.c $(SRC_DIR)/lexer.c $(SRC_DIR)/sema_expr.c \
	$(SRC_DIR)/type.c $(SRC_DIR)/ast.c $(SRC_DIR)/var.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
parser_test:	$(PARSER_SRCS)
	gcc -o $@ $(CFLAGS) $^

VAL_SRCS:=../lib/crt0.c ../examples/util.c valtest.c
valtest:	$(VAL_SRCS) # $(XCC)
	$(XCC) -o$@ $(VAL_SRCS)

.PHONY: test-std-valtest
test-std-valtest:
	$(CC) -Wno-builtin-declaration-mismatch -Wno-overflow ../examples/util.c valtest.c
	./a.out
