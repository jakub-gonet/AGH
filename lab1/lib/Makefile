CC     := gcc
CFLAGS := -Wall -Wextra -g
DIST   := /usr

.PHONY: all install uninstall clean

all: libblock_manager.a libblock_manager.so

install: libblock_manager.so
	mkdir -p $(DIST)/include/bm $(DIST)/lib/bm
	cp block_manager.h $(DIST)/include/bm/block_manager.h
	cp vector.h $(DIST)/include/bm/vector.h
	cp libblock_manager.so $(DIST)/lib/libblock_manager.so

uninstall:
	$(RM) $(DIST)/include/block_manager.h
	$(RM) $(DIST)/include/rrmerge_ptr_vector.h
	$(RM) $(DIST)/lib/libblock_manager.so

libblock_manager.a: block_manager.o vector.h
	$(AR) $(ARFLAGS) libblock_manager.a block_manager.o

block_manager.o: block_manager.c block_manager.h vector.h
	$(CC) $(CFLAGS) block_manager.c -c -o block_manager.o

libblock_manager.so: block_manager.c block_manager.h vector.h
	$(CC) $(CFLAGS) block_manager.c -fPIC -shared -o libblock_manager.so

clean:
	$(RM) *.a *.o *.so