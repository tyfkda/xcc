ROOT_DIR:=../..
LIB_DIR:=$(ROOT_DIR)/lib
EXAMPLES_DIR:=$(ROOT_DIR)/examples

WCC:=../wcc

.PHONY: all
all:	test

.PHONY: test
test:	test-sh test-val
	@echo 'All tests PASS!'

.PHONY: clean
clean:
	rm -f a.wasm valtest.wasm

.PHONY: test-sh
test-sh: # $(WCC)
	@echo '## test.sh'
	WCC="$(WCC)" ./test.sh
	@echo ''

.PHONY: test-val
test-val:	valtest.wasm
	@echo '## valtest'
	node ../runtime/runwasm.js $<
	@echo ''

VAL_SRCS:=$(LIB_DIR)/crt0.c $(ROOT_DIR)/tests/valtest.c $(EXAMPLES_DIR)/example_util.c
valtest.wasm:	$(VAL_SRCS) # $(XCC)
	$(WCC) -o$@ -I$(EXAMPLES_DIR) $^
