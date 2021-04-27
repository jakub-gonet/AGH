#if !defined(COMMON_H)
#define COMMON_H

#include <sys/ipc.h>

#define MAX_CLIENTS 50
#define MAX_MSG_SIZE 100
// #define SERVER_KEY "/home/vscode/server"
// #define CLIENT_KEY "/home/vscode/client"
// #define PROJECT_KEY 'c'

#define SERVER_KEY 0xdead
#define MSG_STRUCT_SIZE (sizeof(struct message_t) - sizeof(long))

typedef int queue_id_t;
typedef unsigned int client_id_t;

enum server_request_t {
  STOP = 10,
  DISCONNECT = 20,
  LIST = 30,
  CONNECT = 40,
  INIT = 50,
  LAST = INIT
};
enum message_source_t { SERVER, CLIENT };

struct message_t {
  long msg_type; /* server_request_t */
  enum message_source_t msg_source;
  union {
    struct init_t {
      queue_id_t queue_id;
      client_id_t client_id;
    } init;
  } payload;
};

#endif  // COMMON_H
