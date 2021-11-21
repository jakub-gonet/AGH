#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "../common/Lfiles.h"

#define BUF_SIZE 8
size_t process_next_line(char* buffer, size_t size);
bool is_square(long n);
void long_to_str(long v, char* buf, size_t* size);

int even_nums = 0;
const char evens_msg[] = "Liczb parzystych jest ";

FILE_HANDLE file_a;
FILE_HANDLE file_b;
FILE_HANDLE file_c;

int main(int argc, char const* argv[]) {
  (void)argc;
  FILE_HANDLE file_in = OPEN_FILE_R(argv[1]);
  file_a = OPEN_FILE_W("a.txt");
  file_b = OPEN_FILE_W("b.txt");
  file_c = OPEN_FILE_W("c.txt");

  char buffer[BUF_SIZE];
  ssize_t n_read;
  size_t curr_line_start = 0;
  while ((n_read = READ_FILE(file_in, &buffer[curr_line_start],
                             BUF_SIZE - curr_line_start - 1)) > 0) {
    buffer[curr_line_start + n_read] = 0;
    curr_line_start = process_next_line(buffer, n_read + curr_line_start);
  }
  // 2147483647 is LONG MAX, so 10 chars + terminating '\n\0'
  char buf[12];
  size_t size;
  long_to_str(even_nums, buf, &size);
  WRITE_FILE(file_a, evens_msg, sizeof(evens_msg) - 1);
  WRITE_FILE(file_a, buf, size);

  CLOSE_FILE(file_in);
  CLOSE_FILE(file_a);
  CLOSE_FILE(file_b);
  CLOSE_FILE(file_c);
}

size_t process_next_line(char* buffer, size_t size) {
  char* end = buffer;

  while (strchr(end, '\n') != NULL) {
    errno = 0;
    const long number = strtol(end, &end, 10);
    if (*end == '\n') {
      ++end;
    }

    assert(errno == 0);
    if (number % 2 == 0) {
      ++even_nums;
    }
    const long remainder = number / 10 % 10;
    if (number >= 10 && (remainder == 0 || remainder == 7)) {
      char buf[12];
      size_t size;
      long_to_str(number, buf, &size);
      WRITE_FILE(file_b, buf, size);
    }
    if (is_square(number)) {
      char buf[12];
      size_t size;
      long_to_str(number, buf, &size);
      WRITE_FILE(file_c, buf, size);
    }
  }
  // copy not handled chars to start of array
  const size_t leftover = size - (end - buffer);
  memmove(buffer, end, leftover);

  return leftover;
}

void long_to_str(long v, char* buf, size_t* size) {
  int mask = 1;
  while (mask * 10 < v) {
    mask *= 10;
  }
  size_t pos = 0;
  while (mask > 0) {
    buf[pos++] = '0' + (v / mask) % 10;
    mask /= 10;
  }
  buf[pos++] = '\n';
  buf[pos] = '\0';
  *size = pos;
}

bool is_square(long n) {
  if (n < 0) {
    return false;
  }
  if (n == 0) {
    return true;
  }

  for (long long i = 1; i * i <= n; ++i) {
    if (n % i == 0 && n / i == i) {
      return true;
    }
  }
  return false;
}