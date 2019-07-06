.PHONY: clean test gen2 gen3 self-hosting

CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
SRC_DIR:=src/cc
CPP_DIR:=src/cpp
OBJ_DIR:=obj
CFLAGS+=-I $(SRC_DIR)

CC_SRCS:=$(SRC_DIR)/util.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/parser.c \
	$(SRC_DIR)/codegen.c $(SRC_DIR)/elfutil.c $(SRC_DIR)/main.c
CPP_SRCS:=$(CPP_DIR)/cpp.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/util.c

CC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CC_SRCS:.c=.o)))
CPP_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(CPP_SRCS:.c=.o)))

all:	xcc cpp

xcc: $(CC_OBJS)
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

test:	all
	$(MAKE) -C tests clean all

test-all: test test-gen2 test-gen3

clean:
	rm -rf xcc cpp $(OBJ_DIR) *~ tmp* a.out gen2 gen3
	$(MAKE) -C tests clean

### Self hosting

gen2: all
	$(MAKE) HOST=. TARGET=gen2 self-hosting
test-gen2: gen2
	$(MAKE) TARGET=gen2 test-self-hosting

gen3: gen2
	$(MAKE) HOST=gen2 TARGET=gen3 self-hosting
test-gen3: gen3
	$(MAKE) TARGET=gen3 test-self-hosting


self-hosting:	$(TARGET)/cpp $(TARGET)/xcc

test-self-hosting:
	$(MAKE) EXEDIR=$(TARGET) -C tests clean all

$(TARGET)/cpp:	$(HOST)/xcc $(HOST)/cpp $(CPP_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$(TARGET)/cpp -Iinc -I$(SRC_DIR) $(CPP_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c

$(TARGET)/xcc:	$(HOST)/xcc $(HOST)/cpp $(CC_SRCS)
	mkdir -p $(TARGET)
	$(HOST)/xcc -o$(TARGET)/xcc -Iinc $(CC_SRCS) \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
