#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char** argv) {
  (void)argc;
  char* end;
  if (argc != 2) {
    printf("Pass n in order to run this program\n");
    exit(EXIT_FAILURE);
  }
  errno = 0;
  const unsigned n = strtol(argv[1], &end, 10);
  assert(errno == 0);
  pid_t child_pid;
  for (size_t i = 0; i < n; i++) {
    child_pid = fork();
    if (child_pid == 0) {
      printf("[%ld] ppid: %d\n", i, (int)getppid());
      return 0;
    }
  }
  while (wait(NULL) > 0)
    ;
  return 0;
}