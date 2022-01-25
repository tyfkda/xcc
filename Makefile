XCC_DIR:=src/xcc
CC1_DIR:=src/cc
CC1_ARCH_DIR:=$(CC1_DIR)/arch
CPP_DIR:=src/cpp
AS_DIR:=src/as
LD_DIR:=src/ld
UTIL_DIR:=src/util
OBJ_DIR:=obj

LIBSRC_DIR:=libsrc
LIBOBJ_DIR:=obj/lib
LIB_DIR:=lib

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

ifneq ("$(TARGET)","")
# Self hosting
HOST_EXES:=$(HOST)xcc $(HOST)cc1 $(HOST)cpp $(HOST)as $(HOST)ld
OBJ_DIR:=$(TARGET)
CC:=./$(HOST)xcc
endif

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body \
	-D_DEFAULT_SOURCE \
	-I$(CC1_DIR) -I$(AS_DIR) -I$(UTIL_DIR) $(OPTIMIZE) \
	-I$(CC1_ARCH_DIR)/$(ARCHTYPE)

XCC_SRCS:=$(wildcard $(XCC_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CC1_SRCS:=$(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/$(ARCHTYPE)/*.c) \
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

$(TARGET)xcc: $(HOST_EXES) $(XCC_OBJS)
	$(CC) -o $@ $(XCC_OBJS) $(LDFLAGS)

$(TARGET)cc1: $(HOST_EXES) $(CC1_OBJS)
	$(CC) -o $@ $(CC1_OBJS) $(LDFLAGS)

$(TARGET)cpp: $(HOST_EXES) $(CPP_OBJS)
	$(CC) -o $@ $(CPP_OBJS) $(LDFLAGS)

$(TARGET)as: $(HOST_EXES) $(AS_OBJS)
	$(CC) -o $@ $(AS_OBJS) $(LDFLAGS)

$(TARGET)ld: $(HOST_EXES) $(LD_OBJS)
	$(CC) -o $@ $(LD_OBJS) $(LDFLAGS)

-include $(OBJ_DIR)/*.d

$(OBJ_DIR)/%.o: $(XCC_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_ARCH_DIR)/$(ARCHTYPE)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CPP_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(AS_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(LD_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(UTIL_DIR)/%.c $(HOST_EXES)
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test
test:	all
	$(MAKE) -C tests clean all

.PHONY: test-all
test-all: test test-gen2 diff-gen23

.PHONY: clean
clean:
	rm -rf cc1 cpp as ld xcc $(OBJ_DIR) $(LIB_DIR) a.out gen2* gen3* tmp.s dump_expr dump_ir dump_type
	$(MAKE) -C tests clean

### Library

ifeq ("$(UNAME)", "Darwin")
LIBS:=
else
LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a
endif

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
	./xcc -c -o $@ $<

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
	$(MAKE) PREFIX=$(TARGET) -C tests clean cc-tests

### Debug

DEBUG_DIR:=src/_debug
DEBUG_CFLAGS:=$(subst -MMD,,$(CFLAGS))

dump_expr:	$(DEBUG_DIR)/dump_expr.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
			$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
	$(CC) -o $@ $(DEBUG_CFLAGS) $^

dump_ir:	$(DEBUG_DIR)/dump_ir.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
			$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(CC1_DIR)/builtin.c \
			$(CC1_DIR)/codegen_expr.c $(CC1_DIR)/codegen.c $(CC1_DIR)/ir.c \
			$(CC1_DIR)/regalloc.c $(CC1_ARCH_DIR)/x64/emit.c $(CC1_ARCH_DIR)/x64/ir_x64.c \
			$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
	$(CC) -o $@ $(DEBUG_CFLAGS) $^

dump_type:	$(DEBUG_DIR)/dump_type.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
			$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
	$(CC) -o $@ $(DEBUG_CFLAGS) $^
