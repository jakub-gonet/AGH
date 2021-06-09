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
  bool is_responding;
  int fd;
  char name[MAX_NAME_SIZE];
  struct game* current_game;
};

struct clients {
  struct client clients[MAX_CLIENTS];
};

struct event_data {
  enum event_type { socket_event, client_event } type;
  union payload {
    int socket;
    struct client* client;
  } payload;
};

// Globals
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int web_socket_fd;
int local_socket_fd;
int epoll_fd;
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

struct client* find_first_empty_client(struct clients* clients_list) {
  for (size_t i = 0; i < MAX_CLIENTS; i++) {
    if (clients_list->clients[i].is_empty) {
      return &clients_list->clients[i];
    }
  }
  return NULL;
}

struct game* find_waiting_oponent(void) {}

bool has_client_by_name(struct clients* clients_list, const char* client_name) {
  for (size_t i = 0; i < MAX_CLIENTS; i++) {
    struct client client = clients_list->clients[i];
    if (client.is_empty) {
      continue;
    }
    if (strcmp(client.name, client_name) == 0) {
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

struct client* register_client_fd(struct clients* clients_list, int client_fd) {
  struct client* client;
  with(mutex) {
    client = find_first_empty_client(clients_list);
    if (client != NULL) {
      printf("Registering client with fd: %d\n", client_fd);
      struct client new_client = {.is_empty = false, .fd = client_fd};
      memcpy(client, &new_client, sizeof(new_client));
    }
  }
  return client;
}

void delete_client(struct client* client) {
  with(mutex) {
    client->is_empty = true;
    // TODO
    // client->current_game.
  }
}

struct clients init_clients(void) {
  struct clients clients_list;
  for (size_t i = 0; i < MAX_CLIENTS; i++) {
    clients_list.clients[i] =
        (struct client){.is_empty = true, .is_responding = true};
  }
  return clients_list;
}

void send_msg(int fd, struct message* msg) {
  int ret = write(fd, msg, sizeof(*msg));
  assert(ret != -1);
}

void read_msg(int fd, struct message* msg) {
  int ret = read(fd, msg, sizeof(*msg));
  assert(ret != -1);
}

void send_ping(int fd) {
  struct message msg = {.type = msg_ping};
  send_msg(fd, &msg);
}

void send_username_taken(int fd) {
  struct message msg = {.type = msg_username_taken};
  printf("Username taken\n");
  send_msg(fd, &msg);
}

void send_server_full(int fd) {
  printf("Server full\n");
  struct message msg = {.type = msg_server_full};
  send_msg(fd, &msg);
}

void handle_client_msg(struct message* msg,
                       struct clients* clients_list,
                       struct client* client) {
  if (msg->type == msg_ping) {
    printf("Ping from %s\n", client->name);
    with(mutex) { client->is_responding = true; }
  } else if (msg->type == msg_register) {
    char* name = msg->payload.registered_name;
    printf("Registering %s\n", name);
    with(mutex) {
      if (has_client_by_name(clients_list, name)) {
        send_username_taken(client->fd);
      } else {
        strcpy(client->name, name);
      }
    }
  } else if (msg->type == msg_move) {
    // TODO
  } else {
    printf("Unknown command: %d\n", msg->type);
    exit(EXIT_FAILURE);
  }
}

void* ping_clients(void* arg) {
  struct clients* clients = arg;
  while (true) {
    sleep(PING_INTERVAL);
    printf("Ping...\n");
    with(mutex) {
      for (size_t i = 0; i < MAX_CLIENTS; i++) {
        struct client client = clients->clients[i];
        if (client.is_empty) {
          continue;
        }
        if (clients->clients[i].is_responding) {
          clients->clients[i].is_responding = false;
          printf("Pinging %s\n", clients->clients[i].name);
          send_ping(clients->clients[i].fd);
        } else {
          delete_client(&clients->clients[i]);
        }
      }
    }
  }
  return NULL;
}

void epoll_client_fd(int fd, struct client* client) {
  struct event_data* event_data = malloc(sizeof(*event_data));
  event_data->type = client_event;
  event_data->payload.client = client;
  struct epoll_event event = {.events = EPOLLIN | EPOLLET,
                              .data.ptr = event_data};
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &event);
}

void listen_to_socket(int socket, void* addr, int addr_size) {
  int ret = bind(socket, (struct sockaddr*)addr, addr_size);
  assert(ret != -1);
  ret = listen(socket, MAX_CLIENTS);
  assert(ret != -1);
  struct event_data* event_data = malloc(sizeof(*event_data));
  event_data->type = socket_event;
  event_data->payload.socket = socket;

  struct epoll_event event = {.events = EPOLLIN | EPOLLPRI,
                              .data.ptr = event_data};
  printf("listen_to_socket: %d\n",
         ((struct event_data*)event.data.ptr)->payload.socket);
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket, &event);
}

int init_web_socket(int port) {
  int web_fd = socket(AF_INET, SOCK_STREAM, 0);
  assert(web_fd != -1);
  struct sockaddr_in web_addr = {
      .sin_family = AF_INET,
      .sin_port = htons(port),
      .sin_addr = {.s_addr = htonl(INADDR_ANY)},
  };
  listen_to_socket(web_fd, &web_addr, sizeof web_addr);
  return web_fd;
}

int init_local_socket(const char* socket_path) {
  unlink(socket_path);
  int local_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  assert(local_fd != -1);
  struct sockaddr_un local_addr = {.sun_family = AF_UNIX};
  strncpy(local_addr.sun_path, socket_path, sizeof local_addr.sun_path);
  listen_to_socket(local_fd, &local_addr, sizeof local_addr);
  return local_fd;
}

void terminate(int signum) {
  UNUSED(signum);
  // noop, make sure that atexit is called
  exit(EXIT_SUCCESS);
}

void on_destroy(void) {
  close(web_socket_fd);
  close(local_socket_fd);
  close(epoll_fd);
  unlink(LOCAL_SOCKET_PATH);
}

int main(int argc, char* argv[]) {
  (void)argc;
  assert(argc == 2 + 1);
  signal(SIGINT, &terminate);
  atexit(&on_destroy);

  srand(time(NULL));

  const int port = to_int(argv[1]);
  const char* socket_path = argv[2];

  epoll_fd = epoll_create(1);
  web_socket_fd = init_web_socket(port);
  local_socket_fd = init_local_socket(socket_path);
  printf("Server listening on port %d and socket %s\n", port, socket_path);

  struct clients clients_list = init_clients();

  pthread_t ping_thread;
  pthread_create(&ping_thread, NULL, &ping_clients, &clients_list);

  while (true) {
    struct epoll_event events[MAX_CLIENTS];
    ssize_t n_ready = epoll_wait(epoll_fd, events, MAX_CLIENTS, -1);
    assert(n_ready >= 0);
    for (size_t i = 0; i < (size_t)n_ready; i++) {
      struct event_data* data = events[i].data.ptr;
      if (data->type == socket_event) {
        printf("socket_event\n");
        int client_fd = accept(data->payload.socket, NULL, NULL);
        assert(client_fd > 0);
        struct client* client = register_client_fd(&clients_list, client_fd);
        if (client != NULL) {
          epoll_client_fd(client_fd, client);
        } else {
          send_server_full(client_fd);
        }
      } else if (data->type == client_event) {
        printf("client_event\n");
        if (events[i].events & EPOLLHUP) {
          printf("Client hung up, removing\n");
          delete_client(data->payload.client);
        } else {
          struct message msg;
          read_msg(data->payload.client->fd, &msg);
          handle_client_msg(&msg, &clients_list, data->payload.client);
        }
      }
    }
  }
  return EXIT_SUCCESS;
}
