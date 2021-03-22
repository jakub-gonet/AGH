#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "../common/Lfiles.h"

#define BUF_SIZE 256
size_t process_next_line(char* buffer, size_t size);

int main(int argc, char const* argv[]) {
  (void)argc;
  FILE_HANDLE file_in = OPEN_FILE_R(argv[1]);
  assert(file_in != FILE_ERR);
  FILE_HANDLE file_out = OPEN_FILE_W(argv[2]);
  assert(file_out != FILE_ERR);

  char buffer[BUF_SIZE];
  ssize_t n_read;
  while ((n_read = READ_FILE(file_in, buffer, BUF_SIZE - 1)) > 0) {
    size_t next_write_i = 0;
    for (size_t i = 0; i < BUF_SIZE - 1; ++i) {
      size_t diff = i - next_write_i + 1;
      if (buffer[i] == '\n') {
        WRITE_FILE(file_out, &buffer[next_write_i], diff);
        next_write_i = i + 1;
      } else if (diff > 50) {
        char tmp = buffer[i];
        buffer[i] = '\n';
        WRITE_FILE(file_out, &buffer[next_write_i], diff);
        buffer[i] = tmp;

        next_write_i = i + 1;
      }
    }
  }
  CLOSE_FILE(file_in);
  CLOSE_FILE(file_out);
}
