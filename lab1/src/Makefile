NAME           := test
CC             := gcc
CFLAGS         := -Wall -Wextra -g
OPT            ?= O2
OUT_DIR        := ./bin
LIB_DIR        := ../lib
TEST_FILES_DIR := ../test_files

SMALL_FILES  := $(TEST_FILES_DIR)/400k_8col.txt:$(TEST_FILES_DIR)/400k_256col.txt
MEDIUM_FILES := $(TEST_FILES_DIR)/10M_b.txt:$(TEST_FILES_DIR)/10M_a.txt \
				$(TEST_FILES_DIR)/4M_128col.txt:$(TEST_FILES_DIR)/4M_256col.txt

MERGE_FILES_CMD   := merge_files $(SMALL_FILES) $(MEDIUM_FILES)
REMOVE_BLOCKS_CMD := remove_block 0 remove_block 1
REMOVE_ROWS_CMD   := remove_row 2 14 remove_row 2 21 remove_row 2 36
TEST_CMD          := $(MERGE_FILES_CMD) $(REMOVE_BLOCKS_CMD) $(REMOVE_ROWS_CMD) $(MERGE_FILES_CMD)

.PHONY: directories all install uninstall clean test $(LIB_DIR)/libblock_manager.a $(LIB_DIR)/libblock_manager.so

all: directories $(OUT_DIR)/$(NAME)_static_$(OPT) $(OUT_DIR)/$(NAME)_shared_$(OPT) $(OUT_DIR)/$(NAME)_dynamic_$(OPT)

install: $(LIB_DIR)/libblock_manager.so
	$(MAKE) -C $(LIB_DIR) install

uninstall:
	$(MAKE) -C $(LIB_DIR) uninstall

$(OUT_DIR)/$(NAME)_static_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.a
	$(CC) $(CFLAGS) -$(OPT) -static $(NAME).c -L $(LIB_DIR)/ -l block_manager -o $(OUT_DIR)/$(NAME)_static_$(OPT)

$(OUT_DIR)/$(NAME)_shared_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.so install
	$(CC) $(CFLAGS) -$(OPT) $(NAME).c -l block_manager -o $(OUT_DIR)/$(NAME)_shared_$(OPT)

$(OUT_DIR)/$(NAME)_dynamic_$(OPT): $(NAME).c $(LIB_DIR)/libblock_manager.so
	$(CC) $(CFLAGS) -$(OPT) $(NAME).c -l dl -DDYNAMIC -o $(OUT_DIR)/$(NAME)_dynamic_$(OPT)

$(LIB_DIR)/libblock_manager.a:
	$(MAKE) -C $(LIB_DIR) libblock_manager.a

$(LIB_DIR)/libblock_manager.so:
	$(MAKE) -C $(LIB_DIR) libblock_manager.so

test: all install
	rm -f results.txt
	@echo 'test static library' | tee -a results.txt
	$(OUT_DIR)/$(NAME)_static_$(OPT) $(TEST_CMD) >> results.txt
	@echo >> results.txt

	@echo 'test shared library'  | tee -a results.txt
	$(OUT_DIR)/$(NAME)_shared_$(OPT) $(TEST_CMD) >> results.txt
	@echo >> results.txt

	@echo 'test dynamic loaded library' | tee -a results.txt
	$(OUT_DIR)/$(NAME)_dynamic_$(OPT) $(TEST_CMD) >> results.txt

directories:
	mkdir -p $(OUT_DIR)
	mkdir -p $(TEST_FILES_DIR)

gen_test_files:
	base64 -w 128 /dev/urandom | head -c 4000000 > $(TEST_FILES_DIR)/4M_128col.txt
	base64 -w 128 /dev/urandom | head -c 4000000 > $(TEST_FILES_DIR)/4M_256col.txt

	base64 -w 128 /dev/urandom | head -c 400000 > $(TEST_FILES_DIR)/400k_8col.txt
	base64 -w 128 /dev/urandom | head -c 400000 > $(TEST_FILES_DIR)/400k_256col.txt

	yes a | head -c 10000000 > $(TEST_FILES_DIR)/10M_a.txt
	yes b | head -c 10000000 > $(TEST_FILES_DIR)/10M_b.txt

clean:
	$(RM) -r $(OUT_DIR)
	$(RM) -r $(TEST_FILES_DIR)
	$(MAKE) -C $(LIB_DIR) clean