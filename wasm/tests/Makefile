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

VAL_SRCS:=../../lib/crt0.c ../../tests/valtest.c ../../examples/example_util.c
valtest.wasm:	$(VAL_SRCS) # $(XCC)
	$(WCC) -o$@ $^
