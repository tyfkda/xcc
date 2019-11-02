XCC_DIR:=src/xcc
CC1_DIR:=src/cc
CPP_DIR:=src/cpp
AS_DIR:=src/as
UTIL_DIR:=src/util
OBJ_DIR:=obj

OPTIMIZE:=-O0 -g3
CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body \
	-D_POSIX_SOURCE
CFLAGS+=-I$(CC1_DIR) -I$(UTIL_DIR) $(OPTIMIZE)

XCC_SRCS:=$(XCC_DIR)/main.c $(UTIL_DIR)/util.c
CC1_SRCS:=$(CC1_DIR)/lexer.c $(CC1_DIR)/type.c $(CC1_DIR)/var.c $(CC1_DIR)/expr.c $(CC1_DIR)/parser.c \
	$(CC1_DIR)/sema.c $(CC1_DIR)/sema_expr.c $(CC1_DIR)/codegen.c $(CC1_DIR)/codegen_expr.c \
	$(CC1_DIR)/ir.c $(CC1_DIR)/emit.c $(CC1_DIR)/cc1.c $(UTIL_DIR)/util.c
CPP_SRCS:=$(CPP_DIR)/cpp.c $(CC1_DIR)/lexer.c $(CC1_DIR)/type.c $(CC1_DIR)/var.c $(CC1_DIR)/expr.c $(CC1_DIR)/sema_expr.c \
	$(UTIL_DIR)/util.c
AS_SRCS:=$(AS_DIR)/as.c $(AS_DIR)/asm_x86.c $(AS_DIR)/parse_asm.c $(AS_DIR)/ir_asm.c $(AS_DIR)/gen.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c

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

-include obj/*.d

$(OBJ_DIR)/%.o: $(XCC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CC1_DIR)/%.c
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
	rm -rf cc1 cpp as $(OBJ_DIR) a.out gen2 gen3 tmp.s
	$(MAKE) -C tests clean

### Self hosting

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
	diff -b gen2/cpp gen3/cpp && diff -b gen2/cc1 gen3/cc1

.PHONY: self-hosting
self-hosting:	$(TARGET)/cpp $(TARGET)/cc1 $(TARGET)/as $(TARGET)/xcc

.PHONY: test-self-hosting
test-self-hosting:
	$(MAKE) EXEDIR=$(TARGET) -C tests clean cc-tests

$(TARGET)/cpp:	$(HOST)/cc1 $(HOST)/cpp $(CPP_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -Iinc -I$(CC1_DIR) -I$(UTIL_DIR) -DSELF_HOSTING $(CPP_SRCS) \
	      lib/lib.c lib/assert.c lib/umalloc.c lib/sprintf.c lib/crt0.c

$(TARGET)/cc1:	$(HOST)/xcc $(CC1_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -Iinc -I$(UTIL_DIR) -DSELF_HOSTING $(CC1_SRCS) \
	      lib/lib.c lib/assert.c lib/umalloc.c lib/sprintf.c lib/crt0.c

$(TARGET)/as:	$(HOST)/xcc $(AS_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -Iinc -I$(UTIL_DIR) -DSELF_HOSTING $(AS_SRCS) \
	      lib/lib.c lib/assert.c lib/umalloc.c lib/sprintf.c lib/crt0.c

$(TARGET)/xcc:	$(HOST)/xcc $(AS_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$@ -Iinc -I$(UTIL_DIR) -DSELF_HOSTING $(XCC_SRCS) \
	      lib/lib.c lib/assert.c lib/umalloc.c lib/sprintf.c lib/crt0.c
