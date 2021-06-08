#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include "common.h"

#define MAX_CLIENTS 16
#define PING_INTERVAL 10

#define with(mutex)                                              \
  for (bool _x = true; _x && (pthread_mutex_lock(&mutex), true); \
       _x = (pthread_mutex_unlock(&mutex), false))

struct game {
  struct client* first_player;
  struct client* second_player;
  area_t area;
};

struct client {
  bool is_empty;
  char name[MAX_NAME_SIZE];
  struct game* current_game;
};

struct clients {
  struct client clients[MAX_CLIENTS];
  size_t size;
};

struct event_data {
  enum event_type { new_connection, client_event } type;
};

// Globals
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int web_socket_fd;
int local_socket_fd;
int epoll_fd;
struct clients clients_list;
// -------

enum cell_type check_for_win(struct game* game) {
  int cross_sum = 0;
  int rev_cross_sum = 0;
  for (size_t y = 0; y < GAME_SIZE; y++) {
    int row_sum = 0;
    int col_sum = 0;
    for (size_t x = 0; x < GAME_SIZE; x++) {
      row_sum += game->area[y][x];
      col_sum += game->area[x][y];
      if (x == y) {
        cross_sum += game->area[y][x];
      }
      if (GAME_SIZE - y - 1 == x) {
        rev_cross_sum += game->area[y][x];
      }
    }

    if (row_sum == -GAME_SIZE || col_sum == -GAME_SIZE ||
        cross_sum == -GAME_SIZE || rev_cross_sum == -GAME_SIZE) {
      return X;
    }
    if (row_sum == GAME_SIZE || col_sum == GAME_SIZE ||
        cross_sum == GAME_SIZE || rev_cross_sum == GAME_SIZE) {
      return O;
    }
  }
  return _;
}

void remove_client_by_name(char* client_name) {}

struct game* find_waiting_oponent(void) {}

bool has_client_by_name(struct clients* clients_list, const char* client_name) {
  for (size_t i = 0; i < clients_list->size; i++) {
    struct client client = clients_list->clients[i];
    if (!client.is_empty && strcmp(client.name, client_name) == 0) {
      return true;
    }
  }
  return false;
}

int to_int(const char* str) {
  errno = 0;
  int val = (int)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

bool register_client(struct clients* clients_list, char* client_name) {
  if (has_client_by_name(clients_list, client_name) ||
      clients_list->size >= MAX_CLIENTS) {
    return false;
  }
  struct client client = {.is_empty = false};
  client.current_game = find_waiting_oponent();
  strncpy(client.name, client_name, sizeof(client.name));
  memcpy(&clients_list->clients[clients_list->size++], &client, sizeof(client));
}

struct clients init_clients(void) {}

void ping_clients(struct clients* clients) {
  static struct message msg = {.type = msg_ping};
  while (true) {
    sleep(PING_INTERVAL);
    printf("Ping...\n");
    with(mutex) {
      // TODO: ping all clients
    }
  }
}

void listen_to_socket(int socket, void* addr, int addr_size) {
  safe(bind(socket, (struct sockaddr*)addr, addr_size));
  safe(listen(socket, MAX_CONN));
  struct epoll_event event = {.events = EPOLLIN | EPOLLPRI};
  // TODO: socket event data
  // event_data* event_data = event.data.ptr = malloc(sizeof *event_data);
  // event_data->type = socket_event;
  // event_data->payload.socket = socket;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket, &event);
}

int init_web_socket(int port) {
  int web_fd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in web_addr = {
      .sin_family = AF_INET,
      .sin_port = htons(port),
      .sin_addr = {.s_addr = htonl(INADDR_ANY)},
  };
  init_socket(web_fd, &web_addr, sizeof web_addr);
  return web_fd;
}

int init_local_socket(char* socket_path) {
  int local_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un local_addr = {.sun_family = AF_UNIX};
  strncpy(local_addr.sun_path, socket_path, sizeof local_addr.sun_path);
  init_socket(local_fd, &local_addr, sizeof local_addr);
  return local_fd;
}

int to_int(char* str) {
  errno = 0;
  int val = (int)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

void terminate(int signum) {
  UNUSED(signum);
  // noop, make sure that atexit is called
  exit(EXIT_SUCCESS);
}

void on_destroy(void) {
  // TODO: disconnect clients first
  close(web_socket_fd);
  close(local_socket_fd);
  close(epoll_fd);
  unlink(LOCAL_SOCKET_PATH);
}

int main(int argc, char* argv[]) {
  (void)argc;
  assert(argc == 3 + 1);
  signal(SIGINT, &terminate);
  atexit(&on_destroy);

  srand(time(NULL));

  const int port = to_int(argv[1]);
  const char* socket_path = argv[2];

  epoll_fd = epoll_create(1);
  web_socket_fd = init_web_socket(port);
  local_socket_fd = init_local_socket(socket_path);
  clients_list = init_clients();

  pthread_t ping_thread;
  pthread_create(&ping_thread, NULL, ping_clients, &clients_list);

  while (true) {
    struct epoll_event events[MAX_CLIENTS];
    ssize_t n_ready = epoll_wait(epoll_fd, events, MAX_CLIENTS, -1);
    assert(n_ready >= 0);
    for (size_t i = 0; i < (size_t)n_ready; i++) {
      struct event_data* data = events[i].data.ptr;
      if (data->type == new_connection) {
        // TODO: create a client and register them
      } else if (data->type == client_event) {
        // TODO: handle client's request
      }
    }
  }
  return EXIT_SUCCESS;
}
