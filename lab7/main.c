#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int p_to_int(char* str) {
  errno = 0;
  int val = (int)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

int main(int argc, char* argv[]) {
  if (argc != 2 + 1) {
    fprintf(stderr, "Specify number of pizza workers and delivery workers\n");
    exit(EXIT_FAILURE);
  }

  int pizza_workers_n = p_to_int(argv[1]);
  int delivery_workers_n = p_to_int(argv[2]);

  return EXIT_SUCCESS;
}
