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

UNAME:=$(shell uname)

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

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body \
	-Wno-gnu-zero-variadic-macro-arguments \
	-D_DEFAULT_SOURCE $(OPTIMIZE) \
	-I$(CC1_DIR) -I$(AS_DIR) -I$(UTIL_DIR) \
	-I$(CC1_ARCH_DIR)
ifneq ("$(NO_FLONUM)","")
CFLAGS+=-D__NO_FLONUM
endif

ifeq ("$(UNAME)", "Darwin")
LIBS:=
else
LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a
endif

ifneq ("$(TARGET)","")
# Self hosting
PARENT_DEPS:=$(HOST)xcc $(HOST)cc1 $(HOST)cpp $(HOST)as $(HOST)ld $(LIBS)
OBJ_DIR:=$(TARGET)
CC:=./$(HOST)xcc
CFLAGS+=-DSELF_HOSTING
endif

XCC_SRCS:=$(wildcard $(XCC_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CC1_SRCS:=$(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CPP_SRCS:=$(wildcard $(CPP_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
AS_SRCS:=$(wildcard $(AS_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c
LD_SRCS:=$(wildcard $(LD_DIR)/*.c) \
	$(AS_DIR)/gen_section.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c

XCC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(XCC_SRCS:.c=.o)))
CC1_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CC1_SRCS:.c=.o)))
CPP_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CPP_SRCS:.c=.o)))
AS_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(AS_SRCS:.c=.o)))
LD_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(LD_SRCS:.c=.o)))

.PHONY: all
all:	exes libs

.PHONY: release
release:
	$(MAKE) OPTIMIZE=-O2

.PHONY:	exes
exes:	$(TARGET)xcc $(TARGET)cc1 $(TARGET)cpp $(TARGET)as $(TARGET)ld

$(TARGET)xcc: $(PARENT_DEPS) $(XCC_OBJS)
	$(CC) -o $@ $(XCC_OBJS) $(LDFLAGS)

$(TARGET)cc1: $(PARENT_DEPS) $(CC1_OBJS)
	$(CC) -o $@ $(CC1_OBJS) $(LDFLAGS)

$(TARGET)cpp: $(PARENT_DEPS) $(CPP_OBJS)
	$(CC) -o $@ $(CPP_OBJS) $(LDFLAGS)

$(TARGET)as: $(PARENT_DEPS) $(AS_OBJS)
	$(CC) -o $@ $(AS_OBJS) $(LDFLAGS)

$(TARGET)ld: $(PARENT_DEPS) $(LD_OBJS)
	$(CC) -o $@ $(LD_OBJS) $(LDFLAGS)

-include $(OBJ_DIR)/*.d

$(OBJ_DIR)/%.o: $(XCC_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_ARCH_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CPP_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(AS_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(LD_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(UTIL_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(DEBUG_DIR)/%.c $(PARENT_DEPS)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test
test:	all
	$(MAKE) -C tests clean && $(MAKE) -C tests all

.PHONY: test-all
test-all: test test-gen2 diff-gen23

.PHONY: clean
clean:
	rm -rf cc1 cpp as ld xcc $(OBJ_DIR) $(LIB_DIR) a.out gen2* gen3* tmp.s \
		dump_expr* dump_ir* dump_type* \
		wcc cc.wasm a.wasm public release
	@$(MAKE) -C tests clean

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
libs: exes $(LIBS)

$(LIB_DIR)/crt0.a:	$(CRT0_OBJS)
	mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIB_DIR)/libc.a:	$(LIBC_OBJS)
	mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIBOBJ_DIR)/%.o: $(LIBSRC_DIR)/**/%.c xcc cc1 cpp as ld
	mkdir -p $(LIBOBJ_DIR)
	./xcc -c -o $@ -Werror $<

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
	diff -b gen2cc1 gen3cc1 && diff -b gen2as gen3as && diff -b gen2cpp gen3cpp && diff -b gen2ld gen3ld && diff -b gen2xcc gen3xcc

.PHONY: self-hosting
self-hosting:	$(TARGET)cpp $(TARGET)cc1 $(TARGET)as $(TARGET)ld $(TARGET)xcc

.PHONY: test-self-hosting
test-self-hosting:	self-hosting
	$(MAKE) PREFIX=$(TARGET) -C tests clean && $(MAKE) PREFIX=$(TARGET) -C tests cc-tests


### Wasm version

WCC_DIR:=src/wcc

WCC_SRCS:=$(wildcard $(WCC_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(CC1_DIR)/type.c $(CC1_DIR)/var.c $(CC1_DIR)/ast.c $(CC1_DIR)/parser.c $(CC1_DIR)/parser_expr.c \
	$(CPP_DIR)/preprocessor.c $(CPP_DIR)/pp_parser.c $(CPP_DIR)/macro.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
WCC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(WCC_SRCS:.c=.o)))

$(OBJ_DIR)/%.o: $(WCC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -I$(CPP_DIR) -c -o $@ $<

wcc: $(PARENT_DEPS) $(WCC_OBJS)
	$(CC) -o $@ $(WCC_OBJS) $(LDFLAGS)

.PHONY: test-wcc
test-wcc:	wcc
	$(MAKE) -C tests clean test-wcc

#### Self hosting

.PHONY: wcc-gen2
wcc-gen2:	wcc
	$(MAKE) HOST_TARGET=wcc HOST_CC="./wcc" WCC_TARGET= wcc-self-hosting
.PHONY: test-wcc-gen2
test-wcc-gen2: wcc-gen2
	$(MAKE) TARGET_CC="node ../tool/runwasm.js ../cc.wasm --" test-wcc-self-hosting

.PHONY: wcc-gen3
wcc-gen3:	wcc-gen2
	$(MAKE) HOST_TARGET=gen2 HOST_CC="node ./tool/runwasm.js ./cc.wasm --" WCC_TARGET=gen3 wcc-self-hosting

.PHONY: wcc-diff-gen23
wcc-diff-gen23:	wcc-gen2 wcc-gen3
	diff -b cc.wasm gen3cc.wasm

.PHONY: wcc-self-hosting
wcc-self-hosting:	$(WCC_TARGET)cc.wasm

.PHONY: test-wcc-self-hosting
test-wcc-self-hosting:
	$(MAKE) WCC="$(TARGET_CC)" -C tests clean test-wcc

WCC_LIBS:=$(LIBSRC_DIR)/_wasm/crt0.c $(LIBSRC_DIR)/_wasm/libc.c

ifeq ("$(WCC_TARGET)", "")
cc.wasm:	$(WCC_SRCS) $(WCC_LIBS) wcc
	./wcc -o $@ \
		-I$(CC1_DIR) -I$(CPP_DIR) -I$(UTIL_DIR) \
		$(WCC_SRCS)
else
$(WCC_TARGET)cc.wasm:	$(WCC_SRCS) $(WCC_LIBS)
	$(HOST_CC) -o $@ \
		-I$(CC1_DIR) -I$(CPP_DIR) -I$(UTIL_DIR) \
		$(WCC_SRCS)
endif

#### www

ASSETS_DIR:=public

.PHONY:	assets
assets:	$(ASSETS_DIR)/cc.wasm $(ASSETS_DIR)/libs.json

$(ASSETS_DIR)/cc.wasm:	cc.wasm
	mkdir -p $(ASSETS_DIR)
	cp cc.wasm $@

$(ASSETS_DIR)/libs.json:	$(WCC_DIR)/www/lib_list.json
	mkdir -p $(ASSETS_DIR)
	node tool/pack_libs.js $(WCC_DIR)/www/lib_list.json > $@

.PHONY: update-wcc-lib
update-wcc-lib:
	find libsrc/* -type d \
		| egrep -v \(crt0\|math\|_wasm\) \
		| while read d; do \
			ls -1 $$d/*.c; \
		  done \
		| sed -e 's/libsrc/../g' \
		| sort \
		| awk '{print "#include \"" $$0 "\""}' \
		> libsrc/_wasm/libc.c
	npx ts-node tool/update_lib_list.ts

.PHONY: release-wcc
release-wcc:	assets
	npm run release


### Debug

DEBUG_CFLAGS:=$(subst -MMD,,$(CFLAGS))

DUMP_EXPR_SRCS:=$(DEBUG_DIR)/dump_expr.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
	$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
DUMP_EXPR_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(DUMP_EXPR_SRCS:.c=.o)))
dump_expr:	$(DUMP_EXPR_OBJS)
	$(CC) -o $@ $(DEBUG_CFLAGS) $^

DUMP_IR_SRCS:=$(DEBUG_DIR)/dump_ir.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
	$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(CC1_DIR)/builtin.c \
	$(CC1_DIR)/codegen_expr.c $(CC1_DIR)/codegen.c $(CC1_DIR)/ir.c $(CC1_DIR)/regalloc.c \
	$(CC1_ARCH_DIR)/emit_code.c $(CC1_DIR)/emit_util.c $(CC1_ARCH_DIR)/ir_$(ARCHTYPE).c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
DUMP_IR_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(DUMP_IR_SRCS:.c=.o)))
dump_ir:	$(DUMP_IR_OBJS)
	$(CC) -o $@ $(DEBUG_CFLAGS) $^

DUMP_TYPE_SRCS:=$(DEBUG_DIR)/dump_type.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
	$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
DUMP_TYPE_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(DUMP_TYPE_SRCS:.c=.o)))
dump_type:	$(DUMP_TYPE_OBJS)
	$(CC) -o $@ $(DEBUG_CFLAGS) $^
