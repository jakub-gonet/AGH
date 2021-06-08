#if !defined(COMMON_H)
#define COMMON_H

#include <stdbool.h>

#define UNUSED(...) (void)(__VA_ARGS__)

#define GAME_SIZE 3
#define MAX_NAME_SIZE 16
#define LOCAL_SOCKET_PATH "local.socket"
#define MAX_CONN 16

enum game_end { TIE, WIN, LOSE };
enum cell_type { _ = 0, X = -1, O = 1 };
typedef enum cell_type area_t[GAME_SIZE][GAME_SIZE];

struct message {
  enum message_type {
    msg_register,
    msg_game_start,
    msg_move,
    msg_new_game_state,
    msg_game_end,
    msg_ping,
    msg_disconnect,
    msg_username_taken,
    msg_server_full
  } type;
  union message_payload {
    struct {
      char oponent_nickname[MAX_NAME_SIZE];
      enum cell_type character;
    } game_start;
    unsigned move;
    enum game_end game_end_state;
    area_t new_area;
    char* registered_name;
  } payload;
};

#endif  // COMMON_H
