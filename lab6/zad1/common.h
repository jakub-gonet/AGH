#if !defined(COMMON_H)
#define COMMON_H

#include <sys/ipc.h>

#define MSG_MAX_CLIENTS 50
#define MSG_MAX_MSG_SIZE 100
#define MSG_SERVER_KEY 0xdeadbeef
#define MSG_STRUCT_SIZE (sizeof(struct msg_message_s) - sizeof(long))

typedef int msg_queue_id_t;
typedef unsigned int msg_client_id_t;

enum msg_server_request_t {
  STOP = 10L,
  DISCONNECT = 20L,
  LIST = 30L,
  CONNECT = 40L,
  INIT = 50L,
  LAST = INIT
};
enum msg_message_source_t { SERVER, CLIENT };

struct msg_message_s {
  long msg_type; /* enum msg_server_request_t */
  enum msg_message_source_t msg_source;
  union {
    struct msg_init_s {
      msg_queue_id_t queue_id;
      msg_client_id_t client_id;
    } init;
  };
};

#endif  // COMMON_H
