CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
SRC_DIR:=src/cc
CPP_DIR:=src/cpp
OBJ_DIR:=obj
OBJS:=$(OBJ_DIR)/util.o $(OBJ_DIR)/lexer.o $(OBJ_DIR)/expr.o $(OBJ_DIR)/parser.o $(OBJ_DIR)/codegen.o $(OBJ_DIR)/elfutil.o $(OBJ_DIR)/main.o
CPPOBJS:=$(OBJ_DIR)/util.o $(OBJ_DIR)/lexer.o $(OBJ_DIR)/expr.o $(OBJ_DIR)/cpp.o

xcc: $(OBJS) cpp
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

cpp: $(CPPOBJS)
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
	rm -rf xcc $(OBJ_DIR) *~ tmp* a.out gen2
	make -C tests clean

### Second generation

gen2: gen2/cpp gen2/xcc

gen2/cpp: xcc cpp src/cpp/cpp.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/util.c
	mkdir -p gen2
	./xcc -ogen2/cpp -Iinc \
	      src/cpp/cpp.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/util.c \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c

gen2/xcc: xcc cpp $(SRC_DIR)/util.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/parser.c $(SRC_DIR)/codegen.c $(SRC_DIR)/elfutil.c $(SRC_DIR)/main.c
	mkdir -p gen2
	./xcc -o$@ -Iinc \
	      $(SRC_DIR)/util.c $(SRC_DIR)/lexer.c $(SRC_DIR)/expr.c $(SRC_DIR)/parser.c $(SRC_DIR)/codegen.c $(SRC_DIR)/elfutil.c $(SRC_DIR)/main.c \
	      lib/lib.c lib/umalloc.c lib/sprintf.c lib/crt0.c
