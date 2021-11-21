#include <assert.h>
#include <stdbool.h>

#include "../common/Lfiles.h"
#include "../common/vector.h"

#define CHUNK_SIZE 256

const char filenames_msg[] = "filenames to merge:\n";

const char* read_stdin();
bool read_up_to(char symbol, FILE_HANDLE file, vec_type(char) * buffer);

int main(int argc, char const* argv[]) {
  const vec_type(char) filenames[] = {argv[1], argv[2]};
  bool read_from_stdin = false;
  if (argc < 3) {
    WRITE_FILE(STDOUT, filenames_msg, sizeof(filenames_msg) - 1);
    filenames[0] = read_stdin();
    filenames[1] = read_stdin();
    read_from_stdin = true;
  }
  FILE_HANDLE file_a = OPEN_FILE_R(filenames[0]);
  assert(file_a != FILE_ERR);
  FILE_HANDLE file_b = OPEN_FILE_R(filenames[1]);
  assert(file_a != FILE_ERR);
  if (read_from_stdin) {
    vec_free(filenames[0]);
    vec_free(filenames[1]);
  }
  vec_type(char) buf_a = NULL;
  vec_type(char) buf_b = NULL;
  bool a_has_lines = false;
  bool b_has_lines = false;
  do {
    a_has_lines = read_up_to('\n', file_a, &buf_a);
    if (a_has_lines) {
      WRITE_FILE(STDOUT, buf_a, vec_get_size(buf_a));
      vec_clear(buf_a);
    }
    b_has_lines = read_up_to('\n', file_b, &buf_b);
    if (b_has_lines) {
      WRITE_FILE(STDOUT, buf_b, vec_get_size(buf_b));
      vec_clear(buf_b);
    }
  } while (a_has_lines || b_has_lines);

  vec_free(buf_a);
  vec_free(buf_b);

  CLOSE_FILE(file_a);
  CLOSE_FILE(file_b);

  return 0;
}

const char* read_stdin() {
  vec_type(char) filename = NULL;
  read_up_to('\n', STDIN, &filename);
  filename[vec_get_size(filename) - 1] = 0;  // remove \n
  return filename;
}

bool read_up_to(char symbol, FILE_HANDLE file, vec_type(char) * buffer) {
  bool has_read = false;
  char buf;
  ssize_t n_read;

  while ((n_read = READ_FILE(file, &buf, 1)) > 0) {
    has_read = true;
    vec_append(*buffer, buf);
    if (buf == symbol) {
      break;
    }
  }

  return has_read;
}