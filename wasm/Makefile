CC1_DIR:=../src/cc
UTIL_DIR:=../src/util
OBJ_DIR:=obj

OPTIMIZE:=-O2 -g3
CFLAGS:=-ansi -std=c11 -MD -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
CFLAGS+=-I$(CC1_DIR) -I$(UTIL_DIR) $(OPTIMIZE)

WCC_DIR:=src
WCC_SRCS:=$(wildcard $(WCC_DIR)/*.c) \
	$(CC1_DIR)/lexer.c $(CC1_DIR)/type.c $(CC1_DIR)/var.c $(CC1_DIR)/ast.c $(CC1_DIR)/parser.c $(CC1_DIR)/parser_expr.c \
	$(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
WCC_OBJS:=$(addprefix $(OBJ_DIR)/,$(notdir $(WCC_SRCS:.c=.o)))

.PHONY: all
all:	wcc

.PHONY: release
release:
	$(MAKE) OPTIMIZE=-O2

wcc: $(WCC_OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

-include $(OBJ_DIR)/*.d

$(OBJ_DIR)/%.o: $(CC1_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(UTIL_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.o: $(WCC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test
test:	all
	$(MAKE) -C tests clean all

.PHONY: test-all
test-all: test test-gen2 diff-gen23

.PHONY: clean
clean:
	rm -rf wcc $(OBJ_DIR) a.wasm
	$(MAKE) -C tests clean
