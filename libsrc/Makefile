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

TESTS=printf stdlib math longjmp

printf_SRCS:=$(wildcard stdio/*.c)
printf_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(printf_SRCS:.c=.o)))

stdlib_SRCS:=$(wildcard stdlib/*.c)
stdlib_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(stdlib_SRCS:.c=.o)))

math_SRCS:=$(wildcard math/*.c)
math_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(math_SRCS:.c=.o)))

longjmp_SRCS:=misc/setjmp.c
longjmp_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(longjmp_SRCS:.c=.o)))

define DEFINE_TEST_TARGET
test-$(1):	$(1)_test
	@echo "## $(1)"
	@./$(1)_test

$(1)_test:	tests/$(1)_test.c $$($(1)_OBJS)
	$(CC) -o$$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $$^
endef
$(foreach D, $(TESTS), $(eval $(call DEFINE_TEST_TARGET,$(D))))

### Wasm version

WCC:=../wcc
WCC_TESTS:=$(TESTS)

.PHONY: test-wcc
test-wcc:	$(foreach D, $(WCC_TESTS), $(addprefix test-wcc-,$(D)))

define DEFINE_WCCTEST_TARGET
.PHONY: test-wcc-$(1)
test-wcc-$(1):	$(1)_test.wasm
	@echo '## $(1) test'
	node --experimental-wasm-eh ../tool/runwasi.js $$<

$(1)_test.wasm:	tests/$(1)_test.c # $(WCC)
	$(WCC) -o$$@ $$^
endef
$(foreach D, $(TESTS), $(eval $(call DEFINE_WCCTEST_TARGET,$(D))))
