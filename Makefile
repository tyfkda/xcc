XCC_DIR:=src/xcc
CC1_DIR:=src/cc
CC1_ARCH_DIR:=$(CC1_DIR)/arch
CPP_DIR:=src/cpp
AS_DIR:=src/as
LD_DIR:=src/ld
UTIL_DIR:=src/util
OBJ_DIR:=obj
LIB_DIR=lib

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body \
	-D_DEFAULT_SOURCE
CFLAGS+=-I$(CC1_DIR) -I$(AS_DIR) -I$(UTIL_DIR) $(OPTIMIZE)
CFLAGS+=-I$(CC1_ARCH_DIR)/x64

XCC_SRCS:=$(wildcard $(XCC_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CC1_SRCS:=$(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/x64/*.c) \
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
exes:	xcc cc1 cpp as ld

xcc: $(XCC_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

cc1: $(CC1_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

cpp: $(CPP_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

as: $(AS_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

ld: $(LD_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

-include $(OBJ_DIR)/*.d

$(OBJ_DIR)/%.o: $(XCC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_ARCH_DIR)/x64/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CPP_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(AS_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(LD_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(UTIL_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test
test:	all
	$(MAKE) -C tests clean all

.PHONY: test-all
test-all: test test-gen2 diff-gen23

.PHONY: clean
clean:
	rm -rf cc1 cpp as ld xcc $(OBJ_DIR) $(LIB_DIR)/*.o a.out gen2* gen3* tmp.s dump_expr dump_ir dump_type
	$(MAKE) -C tests clean

### Library

LIB_SRCS:=$(LIB_DIR)/crt0.c $(LIB_DIR)/libc.c
LIB_OBJS:=$(LIB_SRCS:.c=.o)

libs: exes $(LIB_OBJS)

$(LIB_DIR)/%.o: $(LIB_DIR)/%.c
	./xcc -c -o $@ $<

### Self hosting

.PHONY: gen2
gen2: all
	$(MAKE) HOST= TARGET=gen2 self-hosting
.PHONY: test-gen2
test-gen2: gen2
	$(MAKE) TARGET=gen2 test-self-hosting

.PHONY: gen3
gen3: gen2
	$(MAKE) HOST=gen2 TARGET=gen3 self-hosting

.PHONY: diff-gen23
diff-gen23:	gen2 gen3
	diff -b gen2cc1 gen3cc1 && diff -b gen2as gen3as && diff -b gen2cpp gen3cpp && diff -b gen2ld gen3ld && diff -b gen2xcc gen3xcc

ifneq ("$(TARGET)","")
.PHONY: self-hosting
self-hosting:	$(TARGET)cpp $(TARGET)cc1 $(TARGET)as $(TARGET)ld $(TARGET)xcc

.PHONY: test-self-hosting
test-self-hosting:
	$(MAKE) PREFIX=$(TARGET) -C tests clean cc-tests

HOST_EXES:=$(HOST)xcc $(HOST)cpp $(HOST)cc1 $(HOST)as $(HOST)ld

XCC_GEN_OBJS=$(addprefix $(TARGET)/,$(notdir $(XCC_SRCS:.c=.o)))
CC1_GEN_OBJS=$(addprefix $(TARGET)/,$(notdir $(CC1_SRCS:.c=.o)))
CPP_GEN_OBJS=$(addprefix $(TARGET)/,$(notdir $(CPP_SRCS:.c=.o)))
AS_GEN_OBJS=$(addprefix $(TARGET)/,$(notdir $(AS_SRCS:.c=.o)))
LD_GEN_OBJS=$(addprefix $(TARGET)/,$(notdir $(LD_SRCS:.c=.o)))

$(TARGET)cpp:	$(HOST_EXES) $(CPP_GEN_OBJS)
	./$(HOST)xcc -o$@ $(CPP_GEN_OBJS)

$(TARGET)cc1:	$(HOST_EXES) $(CC1_GEN_OBJS)
	./$(HOST)xcc -o$@ $(CC1_GEN_OBJS)

$(TARGET)as:	$(HOST_EXES) $(AS_GEN_OBJS)
	./$(HOST)xcc -o$@ $(AS_GEN_OBJS)

$(TARGET)ld:	$(HOST_EXES) $(LD_GEN_OBJS)
	./$(HOST)xcc -o$@ $(LD_GEN_OBJS)

$(TARGET)xcc:	$(HOST_EXES) $(XCC_GEN_OBJS)
	./$(HOST)xcc -o$@ $(XCC_GEN_OBJS)

#TARGETGEN_FLAGS:=-DNDEBUG
TARGETGEN_FLAGS:=

$(TARGET)/%.o: $(XCC_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(CC1_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(CC1_ARCH_DIR)/x64 -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(CC1_ARCH_DIR)/x64/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(CC1_ARCH_DIR)/x64 -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(CPP_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(AS_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(LD_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) -I$(AS_DIR) -I$(UTIL_DIR) $(TARGETGEN_FLAGS) $<
$(TARGET)/%.o: $(UTIL_DIR)/%.c
	@mkdir -p $(TARGET)
	./$(HOST)xcc -c -o $@ -I$(CC1_DIR) $(TARGETGEN_FLAGS) $<
endif

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
