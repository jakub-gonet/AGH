NAME    := test
CC      := gcc
CFLAGS  := -Wall -Wextra -g
OPT     ?= O2
OUT_DIR := ./bin
LIB_DIR := ../lib

MERGE_FILES_CMD := merge_files ../test_files/400k_8col.txt:../test_files/400k_256col.txt \
					../test_files/10M_wiet.txt:../test_files/10M_xd.txt \
					../test_files/4M_128col.txt:../test_files/4M_256col.txt \

.PHONY: directories all install uninstall clean test $(LIB_DIR)/libblock_manager.a $(LIB_DIR)/libblock_manager.so

all: directories $(OUT_DIR)/$(NAME)_static_$(OPT) $(OUT_DIR)/$(NAME)_shared_$(OPT) $(OUT_DIR)/$(NAME)_dynamic_$(OPT)

install: $(LIB_DIR)/libblock_manager.so
	$(MAKE) -C $(LIB_DIR) install

uninstall:
	$(MAKE) -C $(LIB_DIR) uninstall

$(OUT_DIR)/$(NAME)_static_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.a
	$(CC) $(CFLAGS) -$(OPT) -static $(NAME).c -L $(LIB_DIR)/ -l block_manager -o $(OUT_DIR)/$(NAME)_static_$(OPT)

$(OUT_DIR)/$(NAME)_shared_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.so
	$(CC) $(CFLAGS) -$(OPT) $(NAME).c -l block_manager -o $(OUT_DIR)/$(NAME)_shared_$(OPT)

$(OUT_DIR)/$(NAME)_dynamic_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.so
	$(CC) $(CFLAGS) -$(OPT) $(NAME).c -l dl -DDYNAMIC -o $(OUT_DIR)/$(NAME)_dynamic_$(OPT)

$(LIB_DIR)/libblock_manager.a:
	$(MAKE) -C $(LIB_DIR) libblock_manager.a

$(LIB_DIR)/libblock_manager.so:
	$(MAKE) -C $(LIB_DIR) libblock_manager.so

test: all install
	rm -f results.txt
	@echo 'test static library'
	$(OUT_DIR)/$(NAME)_static_$(OPT) $(MERGE_FILES_CMD) >> results.txt
	@echo 'test shared library'
	$(OUT_DIR)/$(NAME)_shared_$(OPT) $(MERGE_FILES_CMD) >> results.txt
	@echo 'test dynamic loaded library'
	$(OUT_DIR)/$(NAME)_dynamic_$(OPT) $(MERGE_FILES_CMD) >> results.txt

directories:
	mkdir -p $(OUT_DIR)

clean:
	$(RM) -r $(OUT_DIR)
	$(MAKE) -C $(LIB_DIR) clean