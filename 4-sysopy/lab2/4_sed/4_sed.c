#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "../common/Lfiles.h"

size_t find_prefix_pos(const char* pattern,
                       const char* buffer,
                       const size_t buf_size) {
  size_t pos;
  size_t pattern_i = 0;
  for (size_t i = 0; i < buf_size && pattern[pattern_i] != '\0'; ++i) {
    // match not found
    if (pattern[pattern_i] != buffer[i]) {
      // reset matching progress
      pattern_i = 0;
    }
    // try matching again, if match found
    if (pattern[pattern_i] == buffer[i]) {
      // we set position to found i once
      if (pattern_i == 0) {
        pos = i;
      }
      // and advance to next char in pattern
      ++pattern_i;
    }
  }

  // pattern matched at least once
  if (pattern_i > 0) {
    return pos;
  }
  return buf_size;
}

int main(int argc, char const* argv[]) {
  (void)argc;
  FILE_HANDLE file_in = OPEN_FILE_R(argv[1]);
  assert(file_in != FILE_ERR);
  FILE_HANDLE file_out = OPEN_FILE_W(argv[2]);
  assert(file_out != FILE_ERR);
  const char* pattern = argv[3];
  const char* replacement = argv[4];

  size_t pattern_len = strlen(pattern);
  char* buf = malloc(pattern_len);
  assert(buf != NULL);
  ssize_t n_read;
  size_t pattern_start_i = 0;
  // we're loading chunks of size of the pattern
  while ((n_read = READ_FILE(file_in, &buf[pattern_start_i],
                             pattern_len - pattern_start_i)) > 0) {
    pattern_start_i = find_prefix_pos(pattern, buf, pattern_len);
    // pattern found somewhere later in buffer
    if (pattern_start_i != 0) {
      // write until pattern start
      WRITE_FILE(
          file_out, buf,
          pattern_start_i < (size_t)n_read ? pattern_start_i : (size_t)n_read);
      // shift pattern to start
      const size_t leftover = pattern_len - pattern_start_i;
      memmove(buf, &buf[pattern_start_i], leftover);
      pattern_start_i = leftover;
    } else {
      // we have pattern loaded so we just put replacement and ignore the
      // pattern
      WRITE_FILE(file_out, replacement, strlen(replacement));
      pattern_start_i = 0;
    }
  }

  free(buf);
  CLOSE_FILE(file_in);
  CLOSE_FILE(file_out);
  return 0;
}
