.PHONY: clean test gen2 gen3 test-gen2 gen3 diff-gen23 self-hosting test-self-hosting

SRC_DIR:=src/cc
CPP_DIR:=src/cpp
AS_DIR:=src/as
UTIL_DIR:=src/util
OBJ_DIR:=obj

OPTIMIZE:=-O0 -g3
CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
CFLAGS+=-I$(SRC_DIR) -I$(UTIL_DIR) $(OPTIMIZE)

CC_SRCS:=$(SRC_DIR)/lexer.c $(SRC_DIR)/type.c $(SRC_DIR)/var.c $(SRC_DIR)/expr.c $(SRC_DIR)/analyze.c $(SRC_DIR)/parser.c \
	$(SRC_DIR)/sema.c $(SRC_DIR)/codegen.c $(SRC_DIR)/codegen_expr.c $(SRC_DIR)/main.c \
	$(UTIL_DIR)/util.c
CPP_SRCS:=$(CPP_DIR)/cpp.c $(SRC_DIR)/lexer.c $(SRC_DIR)/type.c $(SRC_DIR)/var.c $(SRC_DIR)/expr.c $(SRC_DIR)/analyze.c \
	$(UTIL_DIR)/util.c
AS_SRCS:=$(AS_DIR)/as.c $(AS_DIR)/gen.c $(UTIL_DIR)/util.c $(UTIL_DIR)/elfutil.c

CC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CC_SRCS:.c=.o)))
CPP_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CPP_SRCS:.c=.o)))
AS_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(AS_SRCS:.c=.o)))

all:	xcc cpp as

release:
	$(MAKE) OPTIMIZE=-O2

xcc: $(CC_OBJS)
	$(CC) -o $@ $(CC_OBJS) $(LDFLAGS)

cpp: $(CPP_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

as: $(AS_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

-include obj/*.d

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
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

test:	all
	$(MAKE) -C tests clean all

test-all: test test-gen2 diff-gen23

clean:
	rm -rf xcc cpp as $(OBJ_DIR) a.out gen2 gen3 tmp.s
	$(MAKE) -C tests clean

### Self hosting

gen2: all
	$(MAKE) HOST=. TARGET=gen2 self-hosting
test-gen2: gen2
	$(MAKE) TARGET=gen2 test-self-hosting

gen3: gen2
	$(MAKE) HOST=gen2 TARGET=gen3 self-hosting

diff-gen23:	gen2 gen3
	diff -b gen2/cpp gen3/cpp && diff -b gen2/xcc gen3/xcc

self-hosting:	$(TARGET)/cpp $(TARGET)/xcc $(TARGET)/as

test-self-hosting:
	$(MAKE) EXEDIR=$(TARGET) -C tests clean cc-tests

$(TARGET)/cpp:	$(HOST)/xcc $(HOST)/cpp $(CPP_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -S -o$@ -Iinc -I$(SRC_DIR) -I$(UTIL_DIR) $(CPP_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
	rm $@
	$(HOST)/as -o$@ $(TARGET)/cpp.s

$(TARGET)/xcc:	$(HOST)/xcc $(HOST)/cpp $(CC_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -S -o$@ -Iinc -I$(UTIL_DIR) $(CC_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
	rm $@
	$(HOST)/as -o$@ $(TARGET)/xcc.s

$(TARGET)/as:	$(HOST)/as $(AS_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -S -o$@ -Iinc -I$(UTIL_DIR) $(AS_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
	rm $@
	$(HOST)/as -o$@ $(TARGET)/as.s
