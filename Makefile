XCC_DIR:=src/xcc
CC1_DIR:=src/cc
CC1_ARCH_DIR:=$(CC1_DIR)/arch
CPP_DIR:=src/cpp
AS_DIR:=src/as
UTIL_DIR:=src/util
OBJ_DIR:=obj

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -pedantic -MMD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
CFLAGS+=-I$(CC1_DIR) -I$(UTIL_DIR) $(OPTIMIZE)
CFLAGS+=-I$(CC1_ARCH_DIR)/x64
CFLAGS+=-D_POSIX_C_SOURCE=200809L  # for getline

XCC_SRCS:=$(wildcard $(XCC_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CC1_SRCS:=$(wildcard $(CC1_DIR)/*.c) \
	$(wildcard $(CC1_ARCH_DIR)/x64/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
CPP_SRCS:=$(wildcard $(CPP_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
AS_SRCS:=$(wildcard $(AS_DIR)/*.c) \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c $(UTIL_DIR)/table.c

XCC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(XCC_SRCS:.c=.o)))
CC1_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CC1_SRCS:.c=.o)))
CPP_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CPP_SRCS:.c=.o)))
AS_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(AS_SRCS:.c=.o)))

.PHONY: all
all:	xcc cc1 cpp as

.PHONY: release
release:
	$(MAKE) OPTIMIZE=-O2

xcc: $(XCC_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

cc1: $(CC1_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

cpp: $(CPP_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

as: $(AS_OBJS)
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
	rm -rf cc1 cpp as xcc $(OBJ_DIR) a.out gen2 gen3 tmp.s dump_expr dump_ir dump_type
	$(MAKE) -C tests clean

### Self hosting

INCLUDE_DIR:=include

.PHONY: gen2
gen2: all
	$(MAKE) HOST=. TARGET=gen2 self-hosting
.PHONY: test-gen2
test-gen2: gen2
	$(MAKE) TARGET=gen2 test-self-hosting

.PHONY: gen3
gen3: gen2
	$(MAKE) HOST=gen2 TARGET=gen3 self-hosting

.PHONY: diff-gen23
diff-gen23:	gen2 gen3
	diff -b gen2/cc1 gen3/cc1 && diff -b gen2/as gen3/as && diff -b gen2/cpp gen3/cpp && diff -b gen2/xcc gen3/xcc

.PHONY: self-hosting
self-hosting:	$(TARGET)/cpp $(TARGET)/cc1 $(TARGET)/as $(TARGET)/xcc

.PHONY: test-self-hosting
test-self-hosting:
	$(MAKE) EXE_DIR=$(TARGET) -C tests clean cc-tests

LIB_SRCS:= lib/lib.c lib/assert.c lib/umalloc.c lib/sprintf.c lib/getopt.c lib/crt0.c

HOST_EXES:=$(HOST)/xcc $(HOST)/cpp $(HOST)/cc1 $(HOST)/as

$(TARGET)/cpp:	$(HOST_EXES) $(CPP_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -I$(INCLUDE_DIR) -I$(CC1_DIR) -I$(UTIL_DIR) -DSELF_HOSTING $(CPP_SRCS) \
	      $(LIB_SRCS)

$(TARGET)/cc1:	$(HOST_EXES) $(CC1_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -I$(INCLUDE_DIR) -I$(CC1_DIR) -I$(CC1_ARCH_DIR)/x64 -I$(UTIL_DIR) -DSELF_HOSTING $(CC1_SRCS) \
	      $(LIB_SRCS)

$(TARGET)/as:	$(HOST_EXES) $(AS_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -I$(INCLUDE_DIR) -I$(UTIL_DIR) -DSELF_HOSTING $(AS_SRCS) \
	      $(LIB_SRCS)

$(TARGET)/xcc:	$(HOST_EXES) $(XCC_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -I$(INCLUDE_DIR) -I$(UTIL_DIR) -DSELF_HOSTING $(XCC_SRCS) \
	      $(LIB_SRCS)

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
