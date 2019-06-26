CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
SRC_DIR:=src/cc
CPP_DIR:=src/cpp
OBJ_DIR:=obj

CC_SRCS:=$(SRC_DIR)/util.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/parser.c \
	$(SRC_DIR)/codegen.c $(SRC_DIR)/elfutil.c $(SRC_DIR)/main.c
CPP_SRCS:=$(CPP_DIR)/cpp.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/util.c

CC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CC_SRCS:.c=.o)))
CPP_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CPP_SRCS:.c=.o)))

xcc: $(CC_OBJS) cpp
	$(CC) -o $@ $(CC_OBJS) $(LDFLAGS)

cpp: $(CPP_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

-include obj/*.d

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(CPP_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

test:	xcc
	make -C tests

clean:
	rm -rf xcc cpp $(OBJ_DIR) *~ tmp* a.out gen2
	make -C tests clean

### Second generation

gen2: gen2/cpp gen2/xcc

gen2/cpp: xcc cpp $(CPP_SRCS)
	mkdir -p gen2
	./xcc -S -o$@ -Iinc $(CPP_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c

gen2/xcc: xcc cpp $(CC_SRCS)
	mkdir -p gen2
	./xcc -S -o$@ -Iinc $(CC_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
