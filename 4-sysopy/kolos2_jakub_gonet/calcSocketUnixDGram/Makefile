CFLAGS = -Wall -lrt
CC = gcc

all: sender receiver

sender: sender.c
	$(CC) $^ -o $@ $(CFLAGS)

receiver: receiver.c
	$(CC) $^ -o $@ $(CFLAGS)

clean:
	rm -f sender.o sender receiver.o receiver

test:  sender receiver
	@echo -e "\nTESTING\n"
	./receiver &
	./sender 22
	./receiver &
	./sender 9
	./receiver &
	./sender -13
	./receiver &
	./sender cos
