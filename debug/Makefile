CC1_DIR:=../src/cc
UTIL_DIR:=../src/util

CFLAGS:=-ansi -std=c11 -Wall -Wextra -Werror -Wold-style-definition \
	-Wno-missing-field-initializers -Wno-typedef-redefinition -Wno-empty-body
CFLAGS+=-I$(CC1_DIR) -I$(UTIL_DIR)
CFLAGS+=-D_POSIX_C_SOURCE=200809L  # for getline

.PHONY: default
default:
	@echo No default target 1>&2
	@exit 1

.PHONY: clean
clean:
	rm -rf dump_expr

dump_expr:	dump_expr.c $(CC1_DIR)/parser_expr.c $(CC1_DIR)/parser.c $(CC1_DIR)/lexer.c \
	$(CC1_DIR)/type.c $(CC1_DIR)/ast.c $(CC1_DIR)/var.c $(UTIL_DIR)/util.c $(UTIL_DIR)/table.c
