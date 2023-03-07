LIBSRC_DIR:=../libsrc
LIBOBJ_DIR:=./obj
LIB_DIR:=../lib
INC_DIR:=../include

LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a

UNAME:=$(shell uname)
ifeq ("$(UNAME)", "Darwin")
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror \
	-Wno-incompatible-library-redeclaration
else
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror \
	-Wno-builtin-declaration-mismatch \
	-Wno-format-truncation
endif

### Library

CRT0_SRCS:=$(wildcard $(LIBSRC_DIR)/crt0/*.c)

LIBC_SRCS:=\
	$(wildcard $(LIBSRC_DIR)/math/*.c) \
	$(wildcard $(LIBSRC_DIR)/misc/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdio/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdlib/*.c) \
	$(wildcard $(LIBSRC_DIR)/string/*.c) \
	$(wildcard $(LIBSRC_DIR)/unistd/*.c) \

CRT0_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(CRT0_SRCS:.c=.o)))
LIBC_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(LIBC_SRCS:.c=.o)))

.PHONY: libs
libs: $(LIBS)

.PHONY: clean
clean:	clean-test
	rm -rf $(LIBOBJ_DIR) $(LIB_DIR)

$(LIB_DIR)/crt0.a:	$(CRT0_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIB_DIR)/libc.a:	$(LIBC_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIBOBJ_DIR)/%.o: $(LIBSRC_DIR)/**/%.c
	@mkdir -p $(LIBOBJ_DIR)
	$(CC) -c -o $@ -Werror -ffreestanding $(CFLAGS) $<

### Test

.PHONY:	test
test:	test-printf test-stdlib test-math test-longjmp

.PHONY: clean-test
clean-test:
	rm -rf printf_test stdlib_test math_test longjmp_test \
		*.wasm

.PHONY: test-printf
test-printf:	printf_test
	@echo '## printf test'
	@./printf_test

STDIO_SRCS:=$(wildcard stdio/*.c)
STDIO_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(STDIO_SRCS:.c=.o)))
printf_test:	tests/printf_test.c $(STDIO_OBJS)
	$(CC) -o$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $^

.PHONY: test-math
test-math:	math_test
	@echo '## math test'
	@./math_test

STDLIB_SRCS:=$(wildcard stdlib/*.c)
STDLIB_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(STDLIB_SRCS:.c=.o)))
stdlib_test:	tests/stdlib_test.c $(STDLIB_OBJS)
	$(CC) -o$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $^

.PHONY: echo-stdlib
echo-stdlib:
	@echo $(STDLIB_SRCS)

MATH_SRCS:=$(wildcard math/*.c)
MATH_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(MATH_SRCS:.c=.o)))
math_test:	tests/math_test.c $(MATH_OBJS)
	$(CC) -o$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $^

.PHONY: test-longjmp
test-longjmp:	longjmp_test
	@echo '## longjmp test'
	@./longjmp_test

LONGJMP_SRCS:=misc/setjmp.c
LONGJMP_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(LONGJMP_SRCS:.c=.o)))
longjmp_test:	tests/longjmp_test.c $(LONGJMP_OBJS)
	$(CC) -o$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $^

.PHONY: test-stdlib
test-stdlib:	stdlib_test
	@echo '## stdlib test'
	@./stdlib_test

### Wasm version

WCC:=../wcc

.PHONY: test-wcc
test-wcc:	test-wcc-printf test-wcc-stdlib test-wcc-math test-wcc-longjmp

.PHONY: test-wcc-printf
test-wcc-printf:	printf_test.wasm
	@echo '## printf test'
	node ../tool/runwasm.js $<

printf_test.wasm:	tests/printf_test.c # $(WCC)
	$(WCC) -o$@ $^

.PHONY: test-wcc-stdlib
test-wcc-stdlib:	stdlib_test.wasm
	@echo '## stdlib test'
	node ../tool/runwasm.js $<

stdlib_test.wasm:	tests/stdlib_test.c # $(WCC)
	$(WCC) -o$@ $^

.PHONY: test-wcc-math
test-wcc-math:	math_test.wasm
	@echo '## math test'
	node ../tool/runwasm.js $<

math_test.wasm:	tests/math_test.c # $(WCC)
	$(WCC) -o$@ $^

.PHONY: test-wcc-longjmp
test-wcc-longjmp:	longjmp_test.wasm
	@echo '## longjmp test'
	node --experimental-wasm-eh ../tool/runwasm.js $<

longjmp_test.wasm:	tests/longjmp_test.c # $(WCC)
	$(WCC) -o$@ $^
