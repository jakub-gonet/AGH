#if !defined(COMMON_H)
#define COMMON_H

#include <sys/ipc.h>

#define MSG_MAX_CLIENTS 50
#define MSG_MAX_MSG_SIZE 100
#define MSG_SERVER_KEY 0xdeadbeef
#define MSG_STRUCT_SIZE (sizeof(struct msg_message_s) - sizeof(long))

typedef int msg_queue_id_t;
typedef ssize_t msg_client_id_t;

enum msg_server_request_t {
  STOP = 10,
  DISCONNECT = 20,
  LIST = 30,
  CONNECT = 40,
  INIT = 50,
  LAST = INIT
};
enum msg_message_source_t { SERVER, CLIENT };

struct msg_message_s {
  long msg_type; /* enum msg_server_request_t */
  enum msg_message_source_t msg_source;
  union {
    struct {
    } list;
    struct {
      msg_queue_id_t queue_id;
      msg_client_id_t client_id;
    } init;
    struct {
      msg_client_id_t client_id;
    } stop, disconnect;
    struct msg_connect_s {
      msg_client_id_t client_id;
      msg_client_id_t id_to_connect;
      msg_queue_id_t peer_queue;
    } connect;
  };
};

#endif  // COMMON_H
