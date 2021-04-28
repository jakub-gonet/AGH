#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

//   perror(strerror(errno));

msg_client_id_t client_id;
msg_queue_id_t server_queue;
msg_queue_id_t queue;
msg_queue_id_t peer_queue;

int msg_init_client_queue(void) {
  const int queue_id = msgget(getpid(), IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

int msg_get_server_queue(void) {
  const int queue_id = msgget(MSG_SERVER_KEY, 0);
  assert(queue_id != -1);
  return queue_id;
}

void msg_receive(const msg_queue_id_t from, struct msg_message_s* msg) {
  int err;
  do {
    err = msgrcv(from, msg, MSG_STRUCT_SIZE, 0, 0);
  } while (err < 0 && errno == EINTR);
  if (errno == EINTR) {
    errno = 0;
  }
  assert(errno == 0);
}

void msg_send_message_to(const msg_queue_id_t queue,
                         const struct msg_message_s* msg) {
  int err;
  do {
    err = msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  } while (err < 0 && errno == EINTR);
  if (errno == EINTR) {
    errno = 0;
  }
  assert(errno == 0);
}

msg_client_id_t msg_send_init_to(const msg_queue_id_t destination,
                                 const msg_queue_id_t own_queue) {
  const struct msg_message_s msg = {
      .msg_type = INIT, .msg_source = CLIENT, .init = {.queue_id = own_queue}};
  msg_send_message_to(destination, &msg);
  struct msg_message_s return_message;
  do {
    msg_receive(own_queue, &return_message);
  } while (return_message.msg_type != INIT ||
           return_message.msg_source != SERVER);
  return return_message.init.client_id;
}

void msg_send_stop_to(const msg_queue_id_t destination) {
  const struct msg_message_s msg = {
      .msg_type = STOP, .msg_source = CLIENT, .stop = {.client_id = client_id}};
  msg_send_message_to(destination, &msg);
}

void msg_send_list_to(const msg_queue_id_t destination) {
  const struct msg_message_s msg = {.msg_type = LIST, .msg_source = CLIENT};
  msg_send_message_to(destination, &msg);
}

void msg_send_connect_to(const msg_queue_id_t destination, int peer_client_id) {
  const struct msg_message_s msg = {
      .msg_type = CONNECT,
      .msg_source = CLIENT,
      .connect = {.client_id = client_id, .id_to_connect = peer_client_id}};
  msg_send_message_to(destination, &msg);
}

void msg_send_disconnect_to(const msg_queue_id_t destination) {
  const struct msg_message_s msg = {.msg_type = DISCONNECT,
                                    .msg_source = CLIENT,
                                    .disconnect.client_id = client_id};
  msg_send_message_to(destination, &msg);
}

bool has_prefix(char* buf, char* prefix) {
  return strncmp(buf, prefix, strlen(prefix)) == 0;
}

void sigio_handler(int signum) {
  (void)signum;
  char* buf = NULL;
  size_t n;
  getline(&buf, &n, stdin);
  if (has_prefix(buf, "LIST")) {
    msg_send_list_to(server_queue);
  } else if (has_prefix(buf, "CONNECT")) {
    int peer_client_id;
    sscanf(buf, "CONNECT %d", &peer_client_id);
    msg_send_connect_to(server_queue, peer_client_id);
  } else if (has_prefix(buf, "DISCONNECT")) {
    msg_send_disconnect_to(server_queue);
  } else if (has_prefix(buf, "STOP")) {
    exit(0);
  } else {
    if (peer_queue != -1) {
      struct msg_message_s msg = {.msg_type = MESSAGE, .msg_source = CLIENT};
      strcpy(msg.message, buf);
      msg_send_message_to(peer_queue, &msg);
    }
  }
}

void exit_handler(void) {
  if (peer_queue != -1) {
    msg_send_disconnect_to(server_queue);
  }
  msg_send_stop_to(server_queue);
}

void sigint_handler(int signum) {
  (void)signum;
  exit(0);
}

int main(void) {
  atexit(exit_handler);
  signal(SIGINT, sigint_handler);

  signal(SIGIO, sigio_handler);
  fcntl(STDIN_FILENO, F_SETOWN, getpid());
  fcntl(STDIN_FILENO, F_SETFL, O_ASYNC);

  queue = msg_init_client_queue();
  server_queue = msg_get_server_queue();
  client_id = msg_send_init_to(server_queue, queue);
  printf("== Creating a client with id: %ld ==\n", client_id);
  peer_queue = -1;
  while (true) {
    struct msg_message_s message;
    msg_receive(queue, &message);
    switch (message.msg_type) {
      case STOP:
        exit(0);
        break;
      case CONNECT:
        peer_queue = message.connect.peer_queue;
        printf("== Connected ==\n");
        break;
      case DISCONNECT:
        peer_queue = -1;
        break;
      case MESSAGE:
        printf("> %s", message.message);
        break;
      default:
        break;
    }
  }
  return 0;
}
