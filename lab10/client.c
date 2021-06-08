#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include "common.h"

#define EVENTS_N 2

// Globals
int socket_fd;
int epoll_fd;

int to_int(char* str) {
  errno = 0;
  int val = (int)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

char* cell_type_to_str(enum cell_type c) {
  return c == _ ? " " : c == X ? "X" : c == O ? "O" : NULL;
}

void print_board(area_t area) {
  printf("+-----------+\n");
  for (size_t y = 0; y < GAME_SIZE; y++) {
    for (size_t x = 0; x < GAME_SIZE; x++) {
      printf("| %s ", cell_type_to_str(area[y][x]));
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

void send_msg(struct message* msg) {
  write(socket_fd, msg, sizeof(*msg));
}

void read_msg(struct message* msg) {
  read(socket_fd, msg, sizeof(*msg));
}

void send_move(int n) {
  struct message msg = {.type = msg_move, .payload.move = n};
  send_msg(&msg);
}

void send_register(char* name) {
  struct message msg = {.type = msg_register};
  strcpy(msg.payload.registered_name, name);
  send_msg(&msg);
}

void send_ping(void) {
  struct message msg = {.type = msg_ping};
  send_msg(&msg);
}

void send_disconnect(void) {}

void handle_msg(struct message* msg, area_t* area) {
  switch (msg->type) {
    case msg_game_start:
      printf("Connected with %s\nYou play with %s\n",
             msg->payload.game_start.oponent_nickname,
             cell_type_to_str(msg->payload.game_start.character));
      print_board(*area);
      break;
    case msg_new_game_state:
      memcpy(area, &msg->payload.new_area, sizeof(msg->payload.new_area));
      print_board(*area);
      break;
    case msg_game_end:
      if (msg->payload.game_end_state == WIN) {
        printf("You won!\n");
      } else if (msg->payload.game_end_state == LOSE) {
        printf("You lost!\n");
      } else if (msg->payload.game_end_state == TIE) {
        printf("You tied!\n");
      }
      exit(EXIT_SUCCESS);
      break;
    case msg_ping:
      send_ping();
      break;
    case msg_username_taken:
    /* fallthrough */
    case msg_server_full:
      /* fallthrough */
    default:
      printf(
          "Server full, username already exists or server issued unknown "
          "command. Exiting\n");
      exit(EXIT_FAILURE);
  }
}

int epoll_init(void) {
  int epoll_fd = epoll_create(1);

  struct epoll_event event = {.events = EPOLLIN | EPOLLPRI,
                              .data = {.fd = STDIN_FILENO}};
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD, STDIN_FILENO, &event);
  event.data.fd = socket_fd;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket_fd, &event);
  return epoll_fd;
}

void terminate(int signum) {
  UNUSED(signum);
  // noop, make sure that atexit is called
  exit(EXIT_SUCCESS);
}

void unregister(void) {
  send_disconnect();
  close(socket_fd);
  close(epoll_fd);
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

  send_register(name);

  area_t area = {0};

  epoll_fd = epoll_init();
  printf("Waiting for oponent...\n");
  while (true) {
    struct epoll_event events[EVENTS_N];
    ssize_t n_ready = epoll_wait(epoll_fd, events, EVENTS_N, -1);
    assert(n_ready >= 0);
    for (size_t i = 0; i < (size_t)n_ready; i++) {
      if (events[i].data.fd == STDIN_FILENO) {
        int cell_n = scanf("%d", &cell_n);
        send_move(cell_n);
      } else if (events[i].data.fd == socket_fd) {
        struct message msg;
        read_msg(&msg);
        handle_msg(&msg, &area);
      }
    }
  }

  return 0;
}
