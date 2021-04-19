#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

void sleep_random_up_to(int ms) {
  usleep((rand() % ms) * 1000);
}

int main(int argc, char const* argv[]) {
  if (argc != 1 + 4) {
    return EXIT_FAILURE;
  }
  srand(time(NULL));

  const char* pipe_path = argv[1];
  const char* line_n = argv[2];
  const char* path = argv[3];
  const unsigned n = strtol(argv[4], NULL, 10);
  assert(errno == 0);

  FILE* pipe = fopen(pipe_path, "w");
  assert(pipe != NULL);
  FILE* file = fopen(path, "r");
  assert(file != NULL);
  char* buffer = malloc(n + 1);
  size_t read_bytes;
  while ((read_bytes = fread(buffer + 1, sizeof(buffer - 1), n, file)) != 0) {
    sleep_random_up_to(500);
    fwrite(line_n, 1, sizeof(line_n), pipe);
    buffer[0] = ' ';
    fwrite(buffer, 1, read_bytes, pipe);
  }
  free(buffer);
  fclose(pipe);
  fclose(file);
  return 0;
}
