CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
SRC_DIR:=src/cc
CPP_DIR:=src/cpp
OBJ_DIR:=obj
OBJS:=$(OBJ_DIR)/util.o $(OBJ_DIR)/lexer.o $(OBJ_DIR)/parser.o $(OBJ_DIR)/codegen.o $(OBJ_DIR)/elfutil.o $(OBJ_DIR)/main.o
CPPOBJS:=$(OBJ_DIR)/util.o $(OBJ_DIR)/lexer.o $(OBJ_DIR)/cpp.o

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
	rm -rf xcc $(OBJ_DIR) *~ tmp*
	make -C tests clean
