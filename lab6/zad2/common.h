#if !defined(COMMON_H)
#define COMMON_H

#include <sys/ipc.h>

#define MSG_MAX_CLIENTS 50
#define MSG_MAX_MSG_SIZE 100
#define MSG_RECEIVED_SIZE 16000
#define MSG_MAX_REQUEST_SIZE 512
#define MSG_SERVER_PATH "/msg-server"
#define MSG_CLIENT_PREFIX "/msg-client-"
#define MSG_STRUCT_SIZE (sizeof(struct msg_message_s) - sizeof(long))

typedef ssize_t msg_client_id_t;

enum msg_server_request_t {
  MESSAGE = 10,
  CONNECT = 20,
  INIT = 30,
  DISCONNECT = 40,
  LIST = 50,
  STOP = 60,
  LAST = STOP
};
enum msg_message_source_t { SERVER, CLIENT };

struct msg_message_s {
  long msg_type; /* enum msg_server_request_t */
  enum msg_message_source_t msg_source;
  union {
    struct {
    } list;
    struct {
      char queue_name[40];
      msg_client_id_t client_id;
    } init;
    struct {
      msg_client_id_t client_id;
    } stop, disconnect;
    struct msg_connect_s {
      msg_client_id_t client_id;
      msg_client_id_t id_to_connect;
      mqd_t peer_queue;
    } connect;
    char message[MSG_MAX_MSG_SIZE];
  };
};

#endif  // COMMON_H
