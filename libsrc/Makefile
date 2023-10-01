SRC_DIR:=.
OBJ_DIR:=./obj
LIB_DIR:=../lib
INC_DIR:=../include

LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a

UNAME:=$(shell uname)
ifeq ("$(UNAME)", "Darwin")
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror \
	-Wno-incompatible-library-redeclaration
else
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror
endif

### Library

CRT0_SRCS:=$(wildcard $(SRC_DIR)/crt0/*.c)

LIBC_SRCS:=\
	$(wildcard $(SRC_DIR)/math/*.c) \
	$(wildcard $(SRC_DIR)/misc/*.c) \
	$(wildcard $(SRC_DIR)/stdio/*.c) \
	$(wildcard $(SRC_DIR)/stdlib/*.c) \
	$(wildcard $(SRC_DIR)/string/*.c) \
	$(wildcard $(SRC_DIR)/unistd/*.c) \

CRT0_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CRT0_SRCS:.c=.o)))
LIBC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(LIBC_SRCS:.c=.o)))

.PHONY: libs
libs: $(LIBS)

.PHONY: clean
clean:	clean-test
	rm -rf $(OBJ_DIR) $(LIB_DIR)

$(LIB_DIR)/crt0.a:	$(CRT0_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIB_DIR)/libc.a:	$(LIBC_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/**/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) -c -o $@ -Werror -ffreestanding $(CFLAGS) $<

### Test

.PHONY:	test
test:	test-printf test-file test-stdlib test-math test-longjmp

.PHONY: clean-test
clean-test:
	rm -rf printf_test file_test stdlib_test math_test longjmp_test \
		a.out *.wasm tmp*

TESTS=printf file stdlib math longjmp

printf_SRCS:=$(wildcard stdio/*.c)
file_SRCS:=$(wildcard stdio/*.c)
stdlib_SRCS:=$(wildcard stdlib/*.c)
math_SRCS:=$(wildcard math/*.c)
longjmp_SRCS:=misc/setjmp.c

define DEFINE_TEST_TARGET
test-$(1):	$(1)_test
	@echo "## $(1)"
	@./$(1)_test

$(1)_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $($(1)_SRCS:.c=.o)))

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
	node --experimental-wasm-eh ../tool/runwasi.js --dir=. $$<

$(1)_test.wasm:	tests/$(1)_test.c # $(WCC)
	$(WCC) -o$$@ $$^
endef
$(foreach D, $(TESTS), $(eval $(call DEFINE_WCCTEST_TARGET,$(D))))
