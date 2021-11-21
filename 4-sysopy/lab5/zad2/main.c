#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CMD_LEN 65536

void build_ps(char* cmd, char* mode) {
  strcpy(cmd, "ps ax -o pid,ppid,user,cmd | sort -k ");
  if (strcmp(mode, "ppid") == 0) {
    strcat(cmd, "2");
  } else if (strcmp(mode, "user") == 0) {
    strcat(cmd, "3");
  }
  strcat(cmd, " | cut -c -80");
}

void build_ping(char* cmd, char* host, char* n) {
  strcpy(cmd, "ping -c ");
  strcat(cmd, n);
  strcat(cmd, " ");
  strcat(cmd, host);
}

int main(int argc, char* argv[]) {
  char cmd[CMD_LEN] = {0};
  if (argc == 1 + 1) {
    build_ps(cmd, argv[1]);
  } else if (argc == 1 + 2) {
    build_ping(cmd, argv[1], argv[2]);
  } else {
    fprintf(stderr, "Wrong number of arguments.");
    exit(EXIT_FAILURE);
  }
  FILE* pipe_fp = popen(cmd, "r");
  assert(pipe_fp != NULL);
  int n;
  char buffer[65536];
  while ((n = fread(buffer, 1, sizeof(buffer), pipe_fp))) {
    fwrite(buffer, 1, n, stdout);
  }
  pclose(pipe_fp);
  return 0;
}
