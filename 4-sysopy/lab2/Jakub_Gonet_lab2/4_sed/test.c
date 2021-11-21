#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

ssize_t find_prefix_pos(const char* pattern,
                        const char* buffer,
                        const size_t buf_size) {
  ssize_t pos;
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

  if (pattern_i > 0) {
    return pos;
  }
  return buf_size;
}

int main(int argc, char const* argv[]) {
  const char* pat = "abcd";
  const char* buf1 = "aaaabcd";
  assert(3 == find_prefix_pos(pat, buf1, strlen(buf1)));
  const char* buf2 = "---abcd";
  assert(3 == find_prefix_pos(pat, buf2, strlen(buf2)));
  const char* buf3 = "abcdefgh";
  assert(0 == find_prefix_pos(pat, buf3, strlen(buf3)));
  const char buf4[] = {'-', '-', 'a', 'b'};
  assert(2 == find_prefix_pos(pat, buf4, 4));
  const char* buf5 = "abcd";
  assert(0 == find_prefix_pos(pat, buf5, strlen(buf5)));
  const char buf6[] = {'x', 'a', 'b', 'c', 'd'};
  assert(1 == find_prefix_pos(pat, buf6, 5));
  const char buf7[] = {'a', 'b', 'c', 'd'};
  assert(0 == find_prefix_pos(pat, buf7, 4));
  const char buf8[] = {'x', 'd'};
  assert(2 == find_prefix_pos(pat, buf8, 2));
  const char* buf9 = "--abcaaaadlapanato";
  assert(strlen(buf9) == find_prefix_pos(pat, buf9, strlen(buf9)));
  return 0;
}
