WCC:=../wcc

.PHONY: all
all:	test

.PHONY: test
test:	test-sh
	@echo 'All tests PASS!'

.PHONY: clean
clean:
	rm -f a.wasm

.PHONY: test-sh
test-sh: # $(WCC)
	@echo '## test.sh'
	WCC="$(WCC)" ./test.sh
	@echo ''
