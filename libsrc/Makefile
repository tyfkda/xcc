LIBSRC_DIR:=../libsrc
LIBOBJ_DIR:=../obj/lib
LIB_DIR:=../lib
INC_DIR:=../include

LIBS:=$(LIB_DIR)/crt0.a $(LIB_DIR)/libc.a

UNAME:=$(shell uname)
ifeq ("$(UNAME)", "Darwin")
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror \
	-Wno-incompatible-library-redeclaration
else
CFLAGS:=-I$(INC_DIR) -Wall -Wextra -Werror \
	-Wno-builtin-declaration-mismatch \
	-Wno-format-truncation
endif

### Library

CRT0_SRCS:=$(wildcard $(LIBSRC_DIR)/crt0/*.c)

LIBC_SRCS:=\
	$(wildcard $(LIBSRC_DIR)/math/*.c) \
	$(wildcard $(LIBSRC_DIR)/misc/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdio/*.c) \
	$(wildcard $(LIBSRC_DIR)/stdlib/*.c) \
	$(wildcard $(LIBSRC_DIR)/string/*.c) \
	$(wildcard $(LIBSRC_DIR)/unistd/*.c) \

CRT0_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(CRT0_SRCS:.c=.o)))
LIBC_OBJS:=$(addprefix $(LIBOBJ_DIR)/,$(notdir $(LIBC_SRCS:.c=.o)))

.PHONY: libs
libs: $(LIBS)

.PHONY: clean
clean:
	rm -rf $(LIBOBJ_DIR) $(LIB_DIR)

$(LIB_DIR)/crt0.a:	$(CRT0_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIB_DIR)/libc.a:	$(LIBC_OBJS)
	@mkdir -p $(LIB_DIR)
	$(AR) r $@ $^

$(LIBOBJ_DIR)/%.o: $(LIBSRC_DIR)/**/%.c
	@mkdir -p $(LIBOBJ_DIR)
	$(CC) -c -o $@ -nostdinc -Werror -ffreestanding $(CFLAGS) $<
