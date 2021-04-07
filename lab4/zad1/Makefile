CC     := gcc
CFLAGS := -g -Wall -Wextra
SHELL  := /bin/bash
RAPORT_F := raport.txt
SIG_NAMES := SIGHUP SIGINT SIGQUIT SIGILL SIGABRT SIGFPE SIGSEGV SIGPIPE SIGALRM SIGTERM SIGCHLD SIGCONT SIGTSTP SIGTTIN SIGTTOU SIGUSR1 SIGUSR2
SIG_NON_MASK_NAMES := SIGSTOP SIGKILL

.PHONY: all test clean

all: main main_exec child

main: main.c flag.h
	$(CC) $(CFLAGS) main.c -o main

main_exec: main_exec.c flag.h
	$(CC) $(CFLAGS) main_exec.c -o main_exec

child: child.c flag.h
	$(CC) $(CFLAGS) child.c -o child

test: main main_exec child
	for name in $(SIG_NAMES) ; do \
		echo "handling $$name" ; \
		echo -e "\nHandling $$name" >> $(RAPORT_F) ;\
		./main_exec ignore $$name >> $(RAPORT_F) ; \
		./main_exec mask $$name >> $(RAPORT_F) ; \
		./main_exec pending $$name >> $(RAPORT_F) ; \
	done
	for name in $(SIG_NAMES) ; do \
		echo "handling $$name" ; \
		echo -e "\nHandling $$name" >> $(RAPORT_F) ;\
		./main ignore $$name >> $(RAPORT_F) ; \
		./main handler $$name >> $(RAPORT_F) ; \
		./main mask $$name >> $(RAPORT_F) ; \
		./main pending $$name >> $(RAPORT_F) ; \
	done


clean:
	$(RM) main main_exec child $(RAPORT_F)
