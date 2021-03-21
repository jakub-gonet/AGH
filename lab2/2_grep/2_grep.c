#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "../common/Lfiles.h"

#define BUF_SIZE 257
size_t write_next_line(const char* buffer, size_t size, char symbol);
int main(int argc, char const* argv[]) {
  (void)argc;
  char buffer[BUF_SIZE];
  const char symbol = argv[1][0];
  FILE_HANDLE file = OPEN_FILE_R(argv[2]);

  ssize_t n_read;
  while ((n_read = READ_FILE(file, &buffer, BUF_SIZE - 1)) > 0) {
    buffer[BUF_SIZE - 1] = 0;
    size_t curr_line_start = 0;
    do {
      curr_line_start += write_next_line(&buffer[curr_line_start],
                                         n_read - curr_line_start, symbol);
    } while (curr_line_start <= (size_t)n_read);
  }

  CLOSE_FILE(file);
}

size_t write_next_line(const char* buffer, size_t size, char symbol) {
  const char* next_symbol_pos = strchr(buffer, symbol);
  const char* next_newline_pos = strchr(buffer, '\n');
  const size_t next_line_boundary = (size_t)(
      next_newline_pos != NULL ? (size_t)(next_newline_pos - buffer) : size);
  if (next_symbol_pos == NULL ||
      next_line_boundary < (size_t)(next_symbol_pos - buffer)) {
    WRITE_FILE(STDOUT, buffer, next_line_boundary + 1);
  }
  return next_line_boundary + 1;
}
