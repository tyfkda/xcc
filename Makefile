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
# NO_BITFIELD:=1
# NO_VLA:=1
# NO_WCHAR:=1

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
ARCHTYPE_UPPER:=$(shell echo "$(ARCHTYPE)" | tr \'[a-z]\' \'[A-Z]\')

AS_ARCH_DIR:=$(AS_DIR)/arch/$(ARCHTYPE)
CC1_ARCH_DIR:=$(CC1_DIR)/arch/$(ARCHTYPE)
CC1_FE_DIR:=$(CC1_DIR)/frontend
CC1_BE_DIR:=$(CC1_DIR)/backend

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-empty-body \
	-D_DEFAULT_SOURCE $(OPTIMIZE) -I$(UTIL_DIR)
ifneq ("$(NO_FLONUM)","")
CFLAGS+=-D__NO_FLONUM
endif
ifneq ("$(NO_BITFIELD)","")
CFLAGS+=-D__NO_BITFIELD
endif
ifneq ("$(NO_VLA)","")
CFLAGS+=-D__NO_VLA
endif
ifneq ("$(NO_WCHAR)","")
CFLAGS+=-D__NO_WCHAR
endif

ifneq ("$(PLATFORM)","")
PLATFORM_UPPER:=$(shell echo "$(PLATFORM)" | tr \'[a-z]\' \'[A-Z]\')
CFLAGS+=-DXCC_TARGET_PLATFORM=XCC_PLATFORM_$(PLATFORM_UPPER)
endif
ifneq ("$(HOST_CC_PREFIX)","")
CFLAGS+=-DHOST_CC_PREFIX=$(HOST_CC_PREFIX)
endif

# For release build:
# CFLAGS+=-DNDEBUG

UNAME:=$(shell uname)
ifeq ("$(UNAME):$(HOST_CC_PREFIX)", "Darwin:")
LIBS:=
else
LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a
endif

# Win32: link necessary platform libraries
PLATFORM_LIBS:=
ifeq ($(OS),Windows_NT)
PLATFORM_LIBS:=-lshlwapi
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
	$(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c
cc1_SRCS:=$(wildcard $(CC1_FE_DIR)/*.c) $(wildcard $(CC1_BE_DIR)/*.c) $(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c
cpp_SRCS:=$(wildcard $(CPP_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c
as_SRCS:=$(wildcard $(AS_DIR)/*.c) \
	$(wildcard $(AS_ARCH_DIR)/*.c) \
	$(UTIL_DIR)/gen_section.c $(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c
ld_SRCS:=$(wildcard $(LD_DIR)/*.c) $(UTIL_DIR)/archive.c \
	$(UTIL_DIR)/gen_section.c $(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c

src_as_CFLAGS:=-I$(AS_DIR) -I$(AS_ARCH_DIR)
src_as_arch_$(ARCHTYPE)_CFLAGS:=-I$(AS_DIR) -I$(AS_ARCH_DIR)
src_cc_CFLAGS:=-I$(CC1_FE_DIR) -I$(CC1_BE_DIR) -I$(CC1_ARCH_DIR)  # arch required for builtin.c
src_cc_frontend_CFLAGS:=-I$(CC1_FE_DIR)
src_cc_backend_CFLAGS:=-I$(CC1_FE_DIR) -I$(CC1_BE_DIR) -I$(CC1_ARCH_DIR)
src_cc_arch_$(ARCHTYPE)_CFLAGS:=-I$(CC1_FE_DIR) -I$(CC1_BE_DIR)
src_cpp_CFLAGS:=-I$(CC1_FE_DIR)
src__debug_CFLAGS:=-I$(CC1_FE_DIR) -I$(CC1_BE_DIR)

.PHONY: all
all:	exes libs

.PHONY: release
release:
	$(MAKE) OPTIMIZE=-O2

.PHONY:	exes
exes:	$(foreach D, $(EXES), $(addprefix $(TARGET),$(D)))

define DEFINE_EXE_TARGET
$(1)_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $($(1)_SRCS:.c=.o)))
$(TARGET)$(1):	$(PARENT_DEPS) $$($(1)_OBJS)
	$(CC) -o $$@ $$($(1)_OBJS) $(LDFLAGS) $(PLATFORM_LIBS)
endef
$(foreach D, $(EXES), $(eval $(call DEFINE_EXE_TARGET,$(D))))

-include $(OBJ_DIR)/*.d

define DEFINE_OBJ_TARGET
$(OBJ_DIR)/%.o: $(1)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -DXCC_TARGET_ARCH=XCC_ARCH_$(ARCHTYPE_UPPER) \
		$$($(subst /,_,$(1))_CFLAGS) \
		-c -o $$@ $$<
endef
XCC_SRC_DIRS:=$(XCC_DIR) $(CC1_FE_DIR) $(CC1_BE_DIR) $(CC1_DIR) $(CC1_ARCH_DIR) $(CPP_DIR) \
	$(AS_DIR) $(AS_ARCH_DIR) $(LD_DIR) $(UTIL_DIR) $(DEBUG_DIR)
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
		wcc wcc-ld cc.wasm a.wasm public release $(WCC_LIBS) $(WCC_OBJ_DIR) $(WCC_LIB_DIR)
	@$(MAKE) -C libsrc clean
	@$(MAKE) -C tests clean

# Run tests on RISC-V simulator.
.PHONY: test-riscv64
test-riscv64:	cross-compile-riscv64
	$(MAKE) -C tests clean && \
		$(MAKE) RUN_EXE="$(CURDIR)/tool/run-riscv64" NO_LINK_TEST=1 -C tests all

.PHONY: cross-compile-riscv64
cross-compile-riscv64:
	$(MAKE) ARCHTYPE:=riscv64 PLATFORM:=posix HOST_CC_PREFIX=riscv64-unknown-elf-

### Library

.PHONY: libs
ifeq ("$(LIBS)", "")
libs: exes
else
libs: exes
	$(MAKE) CC=../xcc HOST_CC_PREFIX=$(HOST_CC_PREFIX) -C libsrc
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

WCC_OBJ_DIR:=obj/wcc
WCC_LIB_DIR:=lib

WCC_DIR:=src/wcc
WCC_CFLAGS:=$(CFLAGS) -I$(CPP_DIR) -I$(CC1_FE_DIR)

ifneq ("$(HOST_TARGET)","")
# Self hosting
WCC_GEN:=$(WCC_TARGET)
ifeq ("$(WCC_GEN)", "")
WCC_GEN:=gen2
endif
WCC_OBJ_DIR:=obj/$(WCC_GEN)wcc
WCC_CFLAGS+=-DSELF_HOSTING
endif

WCC_SRCS:=$(wildcard $(WCC_DIR)/*.c) \
	$(wildcard $(CC1_FE_DIR)/*.c) $(UTIL_DIR)/archive.c \
	$(CPP_DIR)/preprocessor.c $(CPP_DIR)/pp_parser.c $(CPP_DIR)/macro.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c
WCC_OBJS:=$(addprefix $(WCC_OBJ_DIR)/,$(notdir $(WCC_SRCS:.c=.o)))
WCC_LIBS:=$(WCC_LIB_DIR)/wcrt0.a $(WCC_LIB_DIR)/wlibc.a

wcc: $(WCC_OBJS)
	$(CC) -o $@ $(WCC_OBJS) $(LDFLAGS) $(PLATFORM_LIBS)
	$(MAKE) wcc-libs

.PHONY: wcc-libs
wcc-libs:
	$(MAKE) CC=../wcc AR=llvm-ar -C libsrc wcc-libs

WCCLD_SRCS:=$(DEBUG_DIR)/wcc-ld.c $(WCC_DIR)/wasm_linker.c \
	$(WCC_DIR)/wcc_util.c $(WCC_DIR)/emit_wasm.c $(WCC_DIR)/traverse.c $(WCC_DIR)/traverse_setjmp.c \
	$(wildcard $(CC1_FE_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c $(UTIL_DIR)/archive.c
WCCLD_OBJS:=$(addprefix $(WCC_OBJ_DIR)/,$(notdir $(WCCLD_SRCS:.c=.o)))

wcc-ld: $(WCCLD_OBJS)
	$(CC) -o $@ $(WCCLD_OBJS) $(LDFLAGS) $(PLATFORM_LIBS)

-include $(WCC_OBJ_DIR)/*.d

define DEFINE_WCCOBJ_TARGET
$(WCC_OBJ_DIR)/%.o: $(1)/%.c  # $(WCC_PARENT_DEPS)
	@mkdir -p $(WCC_OBJ_DIR)
	$(CC) $(WCC_CFLAGS) -DXCC_TARGET_ARCH=XCC_ARCH_WASM -c -o $$@ $$<
endef
WCC_SRC_DIRS:=$(WCC_DIR) $(DEBUG_DIR) $(CC1_FE_DIR) $(CC1_BE_DIR) $(CC1_DIR) $(CPP_DIR) $(UTIL_DIR)
$(foreach D, $(WCC_SRC_DIRS), $(eval $(call DEFINE_WCCOBJ_TARGET,$(D))))

WCC_CRT0_SRCS:=$(wildcard $(LIBSRC_DIR)/_wasm/crt0/*.c)
WCC_LIBC_SRCS:=$(wildcard $(LIBSRC_DIR)/math/*.c) \
	$(wildcard $(LIBSRC_DIR)/misc/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdio/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdlib/*.c) \
	$(wildcard $(LIBSRC_DIR)/string/*.c) \
	$(wildcard $(LIBSRC_DIR)/_wasm/unistd/*.c)

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

.PHONY: wcc-gen2
wcc-gen2:	wcc
	$(MAKE) HOST_TARGET=wcc HOST_WCC="./wcc" CC="./wcc" WCC_TARGET= wcc-self-hosting
.PHONY: test-wcc-gen2
test-wcc-gen2: wcc-gen2
	$(MAKE) TARGET_CC="../tool/run-gen2wcc.sh" test-wcc-self-hosting

.PHONY: wcc-gen3
wcc-gen3:	wcc-gen2
	$(MAKE) HOST_TARGET=gen2 HOST_WCC="./tool/run-gen2wcc.sh" CC="./tool/run-gen2wcc.sh" \
		WCC_TARGET=gen3 wcc-self-hosting

.PHONY: wcc-diff-gen23
wcc-diff-gen23:	wcc-gen2 wcc-gen3
	diff -b cc.wasm gen3cc.wasm

.PHONY: wcc-self-hosting
wcc-self-hosting:	$(WCC_TARGET)cc.wasm

.PHONY: test-wcc-self-hosting
test-wcc-self-hosting:
	$(MAKE) -C tests clean && $(MAKE) WCC="$(TARGET_CC)" -C tests test-wcc

$(WCC_TARGET)cc.wasm:	$(WCC_OBJS)  # $(WCC_PARENT)
	$(HOST_WCC) -o $@ $(WCC_OBJS)

#### www

ASSETS_DIR:=public

.PHONY:	assets
assets:	$(ASSETS_DIR)/wccfiles.zip

$(ASSETS_DIR)/wccfiles.zip:	$(WCC_DIR)/www/lib_list.json wcc-gen2
	@mkdir -p $(ASSETS_DIR)
	npx ts-node tool/pack_libs.js $(WCC_DIR)/www/lib_list.json $@

.PHONY: release-wcc
release-wcc:	assets
	npm run release


### Debug

DEBUG_EXES:=dump_expr dump_ir dump_type
DEBUG_CFLAGS:=$(subst -MMD,,$(CFLAGS))

dump_expr_SRCS:=$(DEBUG_DIR)/dump_expr.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/fe_misc.c $(CC1_FE_DIR)/initializer.c $(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c \
	$(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c

dump_ir_SRCS:=$(DEBUG_DIR)/dump_ir.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/fe_misc.c $(CC1_FE_DIR)/initializer.c $(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c \
	$(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c $(CC1_FE_DIR)/cc_misc.c \
	$(CC1_BE_DIR)/codegen_expr.c $(CC1_BE_DIR)/codegen.c $(CC1_BE_DIR)/ir.c \
	$(CC1_BE_DIR)/optimize.c $(CC1_BE_DIR)/regalloc.c $(CC1_BE_DIR)/emit_util.c \
	$(CC1_DIR)/builtin.c \
	$(CC1_ARCH_DIR)/emit_code.c $(CC1_ARCH_DIR)/ir_$(ARCHTYPE).c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c

dump_type_SRCS:=$(DEBUG_DIR)/dump_type.c $(CC1_FE_DIR)/parser_expr.c $(CC1_FE_DIR)/parser.c \
	$(CC1_FE_DIR)/fe_misc.c $(CC1_FE_DIR)/initializer.c $(CC1_FE_DIR)/lexer.c $(CC1_FE_DIR)/type.c \
	$(CC1_FE_DIR)/ast.c $(CC1_FE_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/platform.c $(UTIL_DIR)/table.c

define DEFINE_DEBUG_TARGET
$(1)_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $($(1)_SRCS:.c=.o)))
$(1):	$$($(1)_OBJS)
	$(CC) -o $$@ $(DEBUG_CFLAGS) $$^
endef
$(foreach D, $(DEBUG_EXES), $(eval $(call DEFINE_DEBUG_TARGET,$(D))))
