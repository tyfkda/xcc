
.PHONY: test-util test-sh

all:	../xcc test-util test-sh test-examples
	@echo 'All tests PASS!'

../xcc:
	make -C .. xcc

test-util:	util_test
	@echo '## Util'
	@./util_test
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
	gcc -o $@ $^
