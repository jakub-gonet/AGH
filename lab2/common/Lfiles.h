#if !defined(LFILES_H)
#define LFILES_H

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#if defined(LIB)

#define STDOUT stdout
#define STDIN stdin
#define FILE_HANDLE FILE*
#define FILE_ERR NULL
#define OPEN_FILE_R(path) fopen(path, "r")
#define OPEN_FILE_W(path) fopen(path, "w")
#define CLOSE_FILE(file) fclose(file)
#define READ_FILE(file, in_buffer, length) \
  fread(in_buffer, sizeof(char), length, file)
#define WRITE_FILE(file, out_buffer, length) \
  fwrite(out_buffer, sizeof(char), length, file)

#else

#define STDOUT STDOUT_FILENO
#define STDIN STDIN_FILENO
#define FILE_HANDLE int
#define FILE_ERR -1
#define OPEN_FILE_R(path) open(path, O_RDONLY)
#define OPEN_FILE_W(path) open(path, O_WRONLY)
#define CLOSE_FILE(fd) close(fd)
#define READ_FILE(fd, in_buffer, length) read(fd, in_buffer, length)
#define WRITE_FILE(fd, out_buffer, length) write(fd, out_buffer, length)

#endif

#endif  // LFILES_H

// FILE *fopen(const char *pathname, const char *mode);
// int open(const char *pathname, int flags);
// int open(const char *pathname, int flags, mode_t mode);