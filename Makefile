XCC_DIR:=src/xcc
CC1_DIR:=src/cc
CPP_DIR:=src/cpp
AS_DIR:=src/as
LD_DIR:=src/ld
UTIL_DIR:=src/util
DEBUG_DIR:=src/_debug
OBJ_DIR:=obj

LIBSRC_DIR:=libsrc
LIBOBJ_DIR:=obj/lib
LIB_DIR:=lib

# NO_FLONUM:=1

ifeq ("$(ARCHTYPE)", "")
  ARCHTYPE:=x64
  ARCH:=$(shell arch)
  ifeq ("$(ARCH)", "arm64")
    ARCHTYPE:=aarch64
  endif
  ifeq ("$(ARCH)", "aarch64")
    ARCHTYPE:=aarch64
  endif
endif

CC1_ARCH_DIR:=$(CC1_DIR)/arch/$(ARCHTYPE)
CC1_FE_DIR:=$(CC1_DIR)/frontend
CC1_BE_DIR:=$(CC1_DIR)/backend

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body \
	-Wno-gnu-zero-variadic-macro-arguments \
	-D_DEFAULT_SOURCE $(OPTIMIZE) \
	-I$(CC1_FE_DIR) -I$(CC1_BE_DIR) -I$(CC1_ARCH_DIR) -I$(AS_DIR) -I$(UTIL_DIR)
ifneq ("$(NO_FLONUM)","")
CFLAGS+=-D__NO_FLONUM
endif

UNAME:=$(shell uname)
ifeq ("$(UNAME)", "Darwin")
LIBS:=
else
LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a
endif

ifneq ("$(TARGET)","")
# Self hosting
PARENT_DEPS:=$(HOST)xcc $(HOST)cc1 $(HOST)cpp $(HOST)as $(HOST)ld $(LIBS)
OBJ_DIR:=obj/$(TARGET)
CC:=./$(HOST)xcc
CFLAGS+=-DSELF_HOSTING
endif

EXES:=xcc cc1 cpp as ld

xcc_SRCS:=$(wildcard $(XCC_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
cc1_SRCS:=$(wildcard $(CC1_FE_DIR)/*.c) $(wildcard $(CC1_BE_DIR)/*.c) $(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
cpp_SRCS:=$(wildcard $(CPP_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
as_SRCS:=$(wildcard $(AS_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c
ld_SRCS:=$(wildcard $(LD_DIR)/*.c) \
	$(AS_DIR)/gen_section.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c

xcc_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(xcc_SRCS:.c=.o)))
cc1_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(cc1_SRCS:.c=.o)))
cpp_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(cpp_SRCS:.c=.o)))
as_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(as_SRCS:.c=.o)))
ld_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(ld_SRCS:.c=.o)))

.PHONY: all
all:	exes libs

.PHONY: release
release:
	$(MAKE) OPTIMIZE=-O2

.PHONY:	exes
exes:	$(foreach D, $(EXES), $(addprefix $(TARGET),$(D)))

define DEFINE_EXE_TARGET
$(TARGET)$(1):	$(PARENT_DEPS) $$($(1)_OBJS)
	$(CC) -o $$@ $$($(1)_OBJS) $(LDFLAGS)
endef
$(foreach D, $(EXES), $(eval $(call DEFINE_EXE_TARGET,$(D))))

-include $(OBJ_DIR)/*.d

define DEFINE_OBJ_TARGET
$(OBJ_DIR)/%.o: $(1)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $$@ $$<
endef
XCC_SRC_DIRS:=$(XCC_DIR) $(CC1_FE_DIR) $(CC1_BE_DIR) $(CC1_DIR) $(CC1_ARCH_DIR) $(CPP_DIR) \
	$(AS_DIR) $(LD_DIR) $(UTIL_DIR) $(DEBUG_DIR)
$(foreach D, $(XCC_SRC_DIRS), $(eval $(call DEFINE_OBJ_TARGET,$(D))))

.PHONY: test
test:	all
	$(MAKE) -C tests clean && $(MAKE) -C tests all && \
		$(MAKE) test-libs

.PHONY: test-all
test-all: test test-gen2 diff-gen23 test-wcc test-wcc-gen2

.PHONY: test-libs
test-libs:	all
	$(MAKE) -C libsrc clean-test && $(MAKE) CC=../xcc -C libsrc test

.PHONY: clean
clean:
	rm -rf $(EXES) $(OBJ_DIR) a.out gen2* gen3* tmp.s \
		dump_expr* dump_ir* dump_type* \
		wcc cc.wasm a.wasm public release $(WCC_LIBS)
	@$(MAKE) -C libsrc clean
	@$(MAKE) -C tests clean

### Library

.PHONY: libs
ifeq ("$(LIBS)", "")
libs: exes
else
libs: exes
	$(MAKE) CC=../xcc -C libsrc
endif

### Self hosting

.PHONY: gen2
gen2:	all
	$(MAKE) HOST= TARGET=gen2 self-hosting
.PHONY: test-gen2
test-gen2:	gen2
	$(MAKE) TARGET=gen2 test-self-hosting

.PHONY: gen3
gen3: gen2
	$(MAKE) HOST=gen2 TARGET=gen3 self-hosting

.PHONY: diff-gen23
diff-gen23:	gen2 gen3
	diff -b gen2cc1 gen3cc1 && diff -b gen2as gen3as && diff -b gen2cpp gen3cpp && \
		diff -b gen2ld gen3ld && diff -b gen2xcc gen3xcc

.PHONY: self-hosting
self-hosting:	$(TARGET)cpp $(TARGET)cc1 $(TARGET)as $(TARGET)ld $(TARGET)xcc

.PHONY: test-self-hosting
test-self-hosting:	self-hosting
	$(MAKE) PREFIX=$(TARGET) -C tests clean && $(MAKE) PREFIX=$(TARGET) -C tests cc-tests


### Wasm version

ifeq ("$(PARENT_DEPS)","")
WCC_OBJ_DIR:=obj/wcc
else
WCC_OBJ_DIR:=$(OBJ_DIR)
endif

WCC_DIR:=src/wcc
WCC_CFLAGS:=$(CFLAGS) -I$(CPP_DIR) -DTARGET_WASM

WCC_SRCS:=$(wildcard $(WCC_DIR)/*.c) \
	$(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c $(CC1_FE_DIR)/var.c $(CC1_FE_DIR)/ast.c \
	$(CC1_FE_DIR)/parser.c $(CC1_FE_DIR)/parser_expr.c $(CPP_DIR)/preprocessor.c \
	$(CPP_DIR)/pp_parser.c $(CPP_DIR)/macro.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
WCC_OBJS:=$(addprefix $(WCC_OBJ_DIR)/,$(notdir $(WCC_SRCS:.c=.o)))
WCC_LIBS:=$(LIBSRC_DIR)/_wasm/crt0.c $(LIBSRC_DIR)/_wasm/libc.c

wcc: $(PARENT_DEPS) $(WCC_OBJS) $(WCC_LIBS)
	$(CC) -o $@ $(WCC_OBJS) $(LDFLAGS)

define DEFINE_WCCOBJ_TARGET
$(WCC_OBJ_DIR)/%.o: $(1)/%.c $(PARENT_DEPS)
	@mkdir -p $(WCC_OBJ_DIR)
	$(CC) $(WCC_CFLAGS) -c -o $$@ $$<
endef
WCC_SRC_DIRS:=$(WCC_DIR) $(CC1_FE_DIR) $(CC1_BE_DIR) $(CC1_DIR) $(CPP_DIR) $(UTIL_DIR)
$(foreach D, $(WCC_SRC_DIRS), $(eval $(call DEFINE_WCCOBJ_TARGET,$(D))))

WCC_CRT0_SRCS:=$(wildcard $(LIBSRC_DIR)/_wasm/crt0/*.c)
WCC_LIBC_SRCS:=$(wildcard $(LIBSRC_DIR)/math/*.c) \
	$(wildcard $(LIBSRC_DIR)/misc/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdio/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdlib/*.c) \
	$(wildcard $(LIBSRC_DIR)/string/*.c) \
	$(wildcard $(LIBSRC_DIR)/_wasm/unistd/*.c)

$(LIBSRC_DIR)/_wasm/crt0.c:	$(WCC_CRT0_SRCS)
	npx ts-node tool/generate_include_srcs.ts --base=libsrc _wasm/crt0 > $@
$(LIBSRC_DIR)/_wasm/libc.c:	$(WCC_LIBC_SRCS)
	-# Caution: directory order matters.
	npx ts-node tool/generate_include_srcs.ts --base=libsrc \
		math misc stdio stdlib string _wasm/unistd > $@

.PHONY: test-wcc
test-wcc:	wcc
	$(MAKE) -C tests clean && $(MAKE) -C tests test-wcc
	$(MAKE) -C libsrc clean-test && $(MAKE) -C libsrc test-wcc

.PHONY: wcc-on-xcc
wcc-on-xcc:	all
	$(MAKE) CC=./xcc wcc

#### Self hosting

ifeq ("$(HOST_WCC)", "")
  HOST_WCC=./wcc
endif
ifeq ("$(WCC_PARENT)", "")
  WCC_PARENT=wcc
endif

.PHONY: wcc-gen2
wcc-gen2:	wcc
	$(MAKE) HOST_TARGET=wcc WCC_TARGET= wcc-self-hosting
.PHONY: test-wcc-gen2
test-wcc-gen2: wcc-gen2
	$(MAKE) TARGET_CC="../tool/run-gen2wcc.sh" test-wcc-self-hosting

.PHONY: wcc-gen3
wcc-gen3:	wcc-gen2
	$(MAKE) HOST_TARGET=gen2 HOST_WCC="./tool/run-gen2wcc.sh" WCC_TARGET=gen3 WCC_PARENT=cc.wasm \
		wcc-self-hosting

.PHONY: wcc-diff-gen23
wcc-diff-gen23:	wcc-gen2 wcc-gen3
	diff -b cc.wasm gen3cc.wasm

.PHONY: wcc-self-hosting
wcc-self-hosting:	$(WCC_TARGET)cc.wasm

.PHONY: test-wcc-self-hosting
test-wcc-self-hosting:
	$(MAKE) -C tests clean && $(MAKE) WCC="$(TARGET_CC)" -C tests test-wcc

$(WCC_TARGET)cc.wasm:	$(WCC_SRCS) $(WCC_LIBS) $(WCC_PARENT)
	$(HOST_WCC) -o $@ \
		-I$(CC1_FE_DIR) -I$(CPP_DIR) -I$(UTIL_DIR) \
		$(WCC_SRCS)

#### www

ASSETS_DIR:=public

.PHONY:	assets
assets:	$(ASSETS_DIR)/cc.wasm $(ASSETS_DIR)/libs.json

$(ASSETS_DIR)/cc.wasm:	cc.wasm
	@mkdir -p $(ASSETS_DIR)
	cp cc.wasm $@

$(WCC_DIR)/www/lib_list.json:	$(LIBSRC_DIR)/_wasm/crt0.c $(LIBSRC_DIR)/_wasm/libc.c
	npx ts-node tool/update_lib_list.ts --base=./libsrc $^

$(ASSETS_DIR)/libs.json:	$(WCC_DIR)/www/lib_list.json
	@mkdir -p $(ASSETS_DIR)
	node tool/pack_libs.js $(WCC_DIR)/www/lib_list.json > $@

.PHONY: release-wcc
release-wcc:	assets
	npm run release


### Debug

DEBUG_EXES:=dump_expr dump_ir dump_type
DEBUG_CFLAGS:=$(subst -MMD,,$(CFLAGS))

dump_expr_SRCS:=$(DEBUG_DIR)/dump_expr.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c $(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
dump_expr_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(dump_expr_SRCS:.c=.o)))

dump_ir_SRCS:=$(DEBUG_DIR)/dump_ir.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c $(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c \
	$(CC1_BE_DIR)/codegen_expr.c $(CC1_BE_DIR)/codegen.c $(CC1_BE_DIR)/ir.c \
	$(CC1_BE_DIR)/regalloc.c $(CC1_BE_DIR)/emit_util.c $(CC1_ARCH_DIR)/emit_code.c \
	 $(CC1_ARCH_DIR)/ir_$(ARCHTYPE).c $(CC1_DIR)/builtin.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
dump_ir_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(dump_ir_SRCS:.c=.o)))

dump_typ_SRCS:=$(DEBUG_DIR)/dump_type.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c $(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
dump_type_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(dump_type_SRCS:.c=.o)))

define DEFINE_DEBUG_TARGET
$(1):	$$($(1)_OBJS)
	$(CC) -o $$@ $(DEBUG_CFLAGS) $$^
endef
$(foreach D, $(DEBUG_EXES), $(eval $(call DEFINE_DEBUG_TARGET,$(D))))
