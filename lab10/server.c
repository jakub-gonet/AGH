#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CLIENTS 16
#define GAME_SIZE 3

enum cell_type { _, X = -1, O = 1 };

struct game {
  struct client* first_player;
  struct client* second_player;
  char area[GAME_SIZE][GAME_SIZE];
};

struct client {
  char* name;
  struct game* current_game;
};

struct clients {
  struct client* clients[MAX_CLIENTS];
  size_t size;
};

enum cell_type check_for_win(struct game* game) {
  for (size_t y = 0; y < GAME_SIZE; y++) {
    int row_sum = 0;
    int col_sum = 0;
    int cross_sum = 0;
    int rev_cross_sum = 0;
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

    return _;
  }
}

void remove_client_by_name(char* client_name) {}

bool has_client_by_name(struct clients* clients_list, const char* client_name) {
  for (size_t i = 0; i < clients_list->size; i++) {
    if (strcmp(clients_list->clients[i]->name, client_name) == 0) {
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
  struct client* client = malloc(sizeof(struct client));
  client->current_game = find_waiting_oponent();
  client->name = strdup(client_name);
  clients_list->clients[clients_list->size++] = client;
}

struct clients init_clients(void) {}

void free_client(struct client* client) {
  free(client->name);
  free(client);
}

void free_clients(struct clients* clients) {
  for (size_t i = 0; i < clients->size; i++) {
    free_client(clients->clients[i]);
  }
  free(clients);
}

int main(int argc, char* argv[]) {
  (void)argc;
  srand(time(NULL));
  struct clients clients_list = init_clients();

  const int port = to_int(argv[1]);
  const char* socket_path = argv[2];

  free_clients(&clients_list);
  return EXIT_SUCCESS;
}
