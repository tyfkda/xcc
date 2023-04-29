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

UNAME:=$(shell uname)
ifneq ("$(UNAME)", "Darwin")
  CFLAGS+=-no-pie
endif

ifneq ("$(HOST_CC_PREFIX)", "")
  AR:=$(HOST_CC_PREFIX)ar
endif

### Library

CRT0_DIR:=$(SRC_DIR)/crt0
MATH_DIR:=$(SRC_DIR)/math
MISC_DIR:=$(SRC_DIR)/misc
STDIO_DIR:=$(SRC_DIR)/stdio
STDLIB_DIR:=$(SRC_DIR)/stdlib
STRING_DIR:=$(SRC_DIR)/string
UNISTD_DIR:=$(SRC_DIR)/unistd

CRT0_SRCS:=$(wildcard $(CRT0_DIR)/*.c)

LIBC_SRCS:=\
	$(wildcard $(MATH_DIR)/*.c) \
	$(wildcard $(MISC_DIR)/*.c) \
	$(wildcard $(STDIO_DIR)/*.c) \
	$(wildcard $(STDLIB_DIR)/*.c) \
	$(wildcard $(STRING_DIR)/*.c) \
	$(wildcard $(UNISTD_DIR)/*.c) \

CRT0_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CRT0_SRCS:.c=.o)))
LIBC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(LIBC_SRCS:.c=.o)))

.PHONY: libs
libs: $(LIBS)

.PHONY: clean
clean:	clean-test
	rm -rf $(OBJ_DIR) $(LIB_DIR) $(WCC_OBJ_DIR) $(WCC_LIB_DIR)

$(LIB_DIR)/crt0.a:	$(CRT0_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) rc $@ $^

$(LIB_DIR)/libc.a:	$(LIBC_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) rc $@ $^

define DEFINE_OBJ_TARGET
$(OBJ_DIR)/%.o: $(1)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) -c -o $$@ -Wall -Werror -ffreestanding $(CFLAGS) $$<
endef
LIB_SRC_DIRS:=$(CRT0_DIR) $(MATH_DIR) $(MISC_DIR) $(STDIO_DIR) $(STDLIB_DIR) $(STRING_DIR) $(UNISTD_DIR)
$(foreach D, $(LIB_SRC_DIRS), $(eval $(call DEFINE_OBJ_TARGET,$(D))))

### Test

.PHONY:	test
test:	test-printf test-file test-stdlib test-math test-longjmp

.PHONY: clean-test
clean-test:
	rm -rf printf_test file_test stdlib_test math_test longjmp_test \
		a.out *.wasm tmp*

TESTS=printf file stdlib math longjmp

printf_SRCS:=$(wildcard stdio/*.c) \
	math/finite.c math/signbit.c
file_SRCS:=$(wildcard stdio/*.c) \
	math/finite.c math/signbit.c
stdlib_SRCS:=$(wildcard stdlib/*.c)
# stdlib_SRCS:=$(filter-out stdlib/realpath.c,$(wildcard stdlib/*.c))
math_SRCS:=$(wildcard math/*.c)
longjmp_SRCS:=misc/setjmp.c misc/longjmp.c

define DEFINE_TEST_TARGET
test-$(1):	$(1)_test
	@echo "## $(1)"
	@$(RUN_EXE) ./$(1)_test

$(1)_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $($(1)_SRCS:.c=.o)))

$(1)_test:	tests/$(1)_test.c $$($(1)_OBJS)
	$(CC) -o$$@ $(CFLAGS) -I$(INC_DIR) -DUNIT_TEST -ffreestanding $$^
endef
$(foreach D, $(TESTS), $(eval $(call DEFINE_TEST_TARGET,$(D))))

### Wasm version

WCC:=../wcc
WCC_TESTS:=$(TESTS)
WCC_OBJ_DIR:=./obj/wcc
WCC_LIB_DIR:=../lib

WCC_LIBS:=$(WCC_LIB_DIR)/wcrt0.a $(WCC_LIB_DIR)/wlibc.a

WASM_UNISTD_DIR:=$(SRC_DIR)/_wasm/unistd
WASM_CRT0_DIR:=$(SRC_DIR)/_wasm/crt0

WCC_CRT0_SRCS:=$(wildcard $(WASM_CRT0_DIR)/*.c)

WCC_LIBC_SRCS:=\
	$(wildcard $(MATH_DIR)/*.c) \
	$(wildcard $(MISC_DIR)/*.c) \
	$(wildcard $(STDIO_DIR)/*.c) \
	$(wildcard $(STDLIB_DIR)/*.c) \
	$(wildcard $(STRING_DIR)/*.c) \
	$(wildcard $(WASM_UNISTD_DIR)/*.c) \

WCC_CRT0_OBJS:=$(addprefix $(WCC_OBJ_DIR)/,$(notdir $(WCC_CRT0_SRCS:.c=.o)))
WCC_LIBC_OBJS:=$(addprefix $(WCC_OBJ_DIR)/,$(notdir $(WCC_LIBC_SRCS:.c=.o)))

echo-wcc-libc-objs:
	@echo $(WCC_LIBC_OBJS)

$(WCC_LIB_DIR)/wcrt0.a:	$(WCC_CRT0_OBJS)
	@mkdir -p $(WCC_LIB_DIR)
	$(AR) rc $@ $^

$(WCC_LIB_DIR)/wlibc.a:	$(WCC_LIBC_OBJS)
	@mkdir -p $(WCC_LIB_DIR)
	$(AR) rc $@ $^

define DEFINE_WCCOBJ_TARGET
$(WCC_OBJ_DIR)/%.o: $(1)/%.c
	@mkdir -p $(WCC_OBJ_DIR)
	$(CC) -c -o $$@ -Wall -Werror $(WCC_CFLAGS) $$<
endef
WCC_SRC_DIRS:=$(MATH_DIR) $(MISC_DIR) $(STDIO_DIR) $(STDLIB_DIR) $(STRING_DIR) $(WASM_UNISTD_DIR) $(WASM_CRT0_DIR)
$(foreach D, $(WCC_SRC_DIRS), $(eval $(call DEFINE_WCCOBJ_TARGET,$(D))))

.PHONY: wcc-libs
wcc-libs: $(WCC_LIBS)

.PHONY: test-wcc
test-wcc:	$(foreach D, $(WCC_TESTS), $(addprefix test-wcc-,$(D)))

define DEFINE_WCCTEST_TARGET
.PHONY: test-wcc-$(1)
test-wcc-$(1):	$(1)_test.wasm
	@echo '## $(1) test'
	../tool/runwasi --dir=. $$<

$(1)_test.wasm:	tests/$(1)_test.c # $(WCC)
	$(WCC) -o$$@ -Wall -Werror $$^
endef
$(foreach D, $(TESTS), $(eval $(call DEFINE_WCCTEST_TARGET,$(D))))
