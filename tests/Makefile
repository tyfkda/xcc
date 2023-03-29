ROOT_DIR:=..
SRC_DIR:=$(ROOT_DIR)/src
CC_DIR:=$(SRC_DIR)/cc
UTIL_DIR:=$(SRC_DIR)/util
DEBUG_DIR:=$(SRC_DIR)/_debug

CFLAGS:=-ansi -std=c11 -Wall -Wextra -Werror \
	-Wold-style-definition -Wno-missing-field-initializers -Wno-typedef-redefinition \
	-Wno-empty-body
CFLAGS+=-I$(CC_DIR) -I$(UTIL_DIR)
CFLAGS+=-D_POSIX_C_SOURCE=200809L  # for getline

PREFIX:=
XCC:=../$(PREFIX)xcc
CPP:=../$(PREFIX)cpp

.PHONY: all
all:	test

.PHONY: test
test:	unit-tests cpp-tests cc-tests misc-tests
	@echo 'All tests PASS!'

.PHONY: unit-tests
unit-tests:	test-table test-util test-parser test-initializer test-print-type

.PHONY: cpp-tests
cpp-tests:	test-cpp

.PHONY: cc-tests
cc-tests:	test-sh test-val test-dval test-fval

.PHONY: misc-tests
misc-tests:	test-link test-examples

.PHONY: clean
clean:
	rm -f table_test util_test parser_test initializer_test print_type_test \
		valtest dvaltest fvaltest link_test \
		a.out tmp* *.o mandelbrot.ppm \
		*.wasm

.PHONY: test-initializer
test-initializer:	initializer_test
	@echo '## Initializer'
	@./initializer_test

.PHONY: test-table
test-table:	table_test
	@echo '## NameTable'
	@./table_test

.PHONY: test-util
test-util:	util_test
	@echo '## Util'
	@./util_test

.PHONY: test-parser
test-parser:	parser_test
	@echo '## Parser'
	@./parser_test

.PHONY: test-print-type
test-print-type:	print_type_test
	@echo '## Print type'
	@./print_type_test

.PHONY: test-val
test-val:	valtest
	@echo '## valtest'
	@./valtest

.PHONY: test-dval, test-fval
test-dval:	dvaltest
	@echo '## dvaltest'
	@./dvaltest
test-fval:	fvaltest
	@echo '## fvaltest'
	@./fvaltest

.PHONY: test-cpp
test-cpp: # $(CPP)
	@echo '## cpptest'
	@CPP=$(CPP) ./cpptest.sh

.PHONY: test-sh
test-sh: # $(XCC)
	@echo '## test.sh'
	@XCC=$(XCC) ./test.sh
	@rm -f core

.PHONY: test-examples
test-examples: # $(XCC)
	@echo '## Example test'
	@XCC=$(XCC) ./example_test.sh

.PHONY: test-link
test-link: link_test # $(XCC)
	@echo '## Link test'
	@./link_test

INITIALIZER_SRCS:=initializer_test.c $(CC_DIR)/parser.c $(CC_DIR)/parser_expr.c $(CC_DIR)/lexer.c \
	$(CC_DIR)/var.c $(CC_DIR)/type.c $(CC_DIR)/ast.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c \
	$(DEBUG_DIR)/dump_expr.c
initializer_test:	$(INITIALIZER_SRCS)
	$(CC) -o$@ -DNO_MAIN_DUMP_EXPR $(CFLAGS) $^

TABLE_SRCS:=table_test.c $(UTIL_DIR)/table.c
table_test:	$(TABLE_SRCS)
	$(CC) -o$@ $(CFLAGS) $^

UTIL_SRCS:=util_test.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
util_test:	$(UTIL_SRCS)
	$(CC) -o$@ $(CFLAGS) $^

PARSER_SRCS:=parser_test.c $(CC_DIR)/parser_expr.c $(CC_DIR)/lexer.c $(CC_DIR)/parser.c \
	$(CC_DIR)/type.c $(CC_DIR)/ast.c $(CC_DIR)/var.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
parser_test:	$(PARSER_SRCS)
	$(CC) -o$@ $(CFLAGS) $^

VAL_SRCS:=valtest.c
valtest:	$(VAL_SRCS) # $(XCC)
	$(XCC) -o$@ -Werror $^

FVAL_SRCS:=fvaltest.c
dvaltest:	$(FVAL_SRCS) flotest.inc # $(XCC)
	$(XCC) -o$@ -Werror $(FVAL_SRCS)
fvaltest:	$(FVAL_SRCS) flotest.inc # $(XCC)
	$(XCC) -o$@ -Werror -DUSE_SINGLE $(FVAL_SRCS)

TYPE_SRCS:=print_type_test.c $(CC_DIR)/type.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
print_type_test:	$(TYPE_SRCS)
	$(CC) -o $@ $(CFLAGS) $^

.PHONY: test-link
link_test: link_main.c link_sub.c
	$(XCC) -c -olink_main.o -Werror link_main.c
	$(CC) -o $@ link_sub.c link_main.o

.PHONY: test-std-valtest
test-std-valtest:
	$(CC) -Wno-builtin-declaration-mismatch -Wno-overflow valtest.c
	./a.out

### Wasm version

WCC:=../wcc

.PHONY: test-wcc
test-wcc:	test-wcc-sh test-wcc-val test-wcc-dval test-wcc-fval
	@echo 'All tests PASS!'

.PHONY: test-wcc-sh
test-wcc-sh:
	@echo '## test.sh'
	@$(eval AWASM := $(shell basename `mktemp -u`).wasm)
	@XCC="$(WCC)" RE_SKIP='\/\/-WCC' \
		AOUT="$(AWASM)" RUN_AOUT="node ../tool/runwasm.js $(AWASM)" \
		./test.sh

.PHONY: test-wcc-val
test-wcc-val:	valtest.wasm
	@echo '## valtest'
	node ../tool/runwasm.js $<

valtest.wasm:	$(VAL_SRCS) # $(WCC)
	$(WCC) -o$@ $^

.PHONY: test-wcc-dval
test-wcc-dval:	dvaltest.wasm
	@echo '## dvaltest'
	node ../tool/runwasm.js $<

dvaltest.wasm:	$(FVAL_SRCS) # $(WCC)
	$(WCC) -o$@ $^

.PHONY: test-wcc-fval
test-wcc-fval:	fvaltest.wasm
	@echo '## fvaltest'
	node ../tool/runwasm.js $<

fvaltest.wasm:	$(FVAL_SRCS) # $(WCC)
	$(WCC) -o$@ -DUSE_SINGLE $^
