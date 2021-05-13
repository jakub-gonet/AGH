CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
BUILD_DIR := ./bin

.PHONY: all test clean

all: main delivery_worker pizza_worker

main: $(BUILD_DIR)/common.o $(BUILD_DIR)/main.o
	$(CC) $(CFLAGS) $(BUILD_DIR)/common.o $(BUILD_DIR)/main.o -o main -lm

delivery_worker: $(BUILD_DIR)/delivery_worker.o
	$(CC) $(CFLAGS) $(BUILD_DIR)/common.o $(BUILD_DIR)/delivery_worker.o -o delivery_worker -lm

pizza_worker: $(BUILD_DIR)/pizza_worker.o
	$(CC) $(CFLAGS) $(BUILD_DIR)/common.o $(BUILD_DIR)/pizza_worker.o -o pizza_worker -lm

$(BUILD_DIR)/main.o: main.c bin
	$(CC) $(CFLAGS) -c main.c -o $(BUILD_DIR)/main.o

$(BUILD_DIR)/delivery_worker.o: delivery_worker.c bin
	$(CC) $(CFLAGS) -c delivery_worker.c -o $(BUILD_DIR)/delivery_worker.o

$(BUILD_DIR)/pizza_worker.o: pizza_worker.c bin
	$(CC) $(CFLAGS) -c pizza_worker.c -o $(BUILD_DIR)/pizza_worker.o

$(BUILD_DIR)/common.o: common.h common.c bin
	$(CC) $(CFLAGS) -c common.c -o $(BUILD_DIR)/common.o

bin:
	mkdir $(BUILD_DIR)

clean:
	$(RM) bin/* main delivery_worker pizza_worker
