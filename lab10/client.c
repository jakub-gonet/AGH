#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "common.h"

// Globals
int socket_fd;

int to_int(char* str) {
  errno = 0;
  int val = (int)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

void print_board(area_t area) {
  printf("+-----------+\n");
  for (size_t y = 0; y < GAME_SIZE; y++) {
    for (size_t x = 0; x < GAME_SIZE; x++) {
      enum cell_type c = area[y][x];
      printf("| %s ", c == _ ? " " : c == X ? "X" : c == O ? "O" : "$");
    }
    printf("|\n");
    if (y != GAME_SIZE - 1) {
      printf("+---+---+---+\n");
    }
  }
  printf("+-----------+\n");
}

int connect_web(char* connection_path) {
  char* tokenized_port = strtok(connection_path, ":");
  int port = to_int(tokenized_port);
  struct sockaddr_in addr = {.sin_family = AF_INET,
                             // network byte ordered port
                             .sin_port = htons((uint16_t)port)};
  // convert ip v4 to binary
  int ret = inet_pton(AF_INET, connection_path, &addr.sin_addr);
  assert(ret != 0);
  // create endpoint
  int sock = socket(AF_INET, SOCK_STREAM, 0);
  assert(sock > 0);
  // connect
  ret = connect(sock, (struct sockaddr*)&addr, sizeof addr);
  assert(ret != -1);
  return sock;
}

int connect_unix(char* connection_path) {
  struct sockaddr_un addr = {.sun_family = AF_UNIX};
  strcpy(addr.sun_path, connection_path);
  // create endpoint
  int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  assert(sock > 0);
  int ret = connect(sock, (struct sockaddr*)&addr, sizeof addr);
  assert(ret != -1);
  return sock;
}

void terminate(int signum) {
  UNUSED(signum);
  exit(EXIT_SUCCESS);
}

void unregister(void) {
  // TODO
  printf("Goodbye!\n");
}

int main(int argc, char* argv[]) {
  assert(argc == 3 + 1);
  signal(SIGINT, &terminate);
  atexit(&unregister);

  char* name = argv[1];
  char* mode = argv[2];
  char* connection_path = argv[3];

  if (strcmp(mode, "web") == 0) {
    socket_fd = connect_web(connection_path);
  } else if (strcmp(mode, "unix") == 0) {
    socket_fd = connect_unix(connection_path);
  } else {
    printf("Usage: [name] [web|unix] [ipv4:port | socket path]\n");
    exit(EXIT_FAILURE);
  }

  // printf("Waiting for oponent...\n");
  // printf("Connected with %s\n", NAME);
  return 0;
}
