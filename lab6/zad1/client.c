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
  printf("Creating a client with a key %d\n", getpid());
  const int queue_id = msgget(getpid(), IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

int msg_get_server_queue(void) {
  const int queue_id = msgget(MSG_SERVER_KEY, 0);
  assert(queue_id != -1);
  return queue_id;
}

bool msg_receive(const msg_queue_id_t from, struct msg_message_s* msg) {
  const int res = msgrcv(from, msg, MSG_STRUCT_SIZE, 0, 0);
  if (res < 0) {
    if (errno == EIDRM) {
      return false;
    }
    // perror(strerror(errno));
  }
  return true;
}

bool msg_send_message_to(const msg_queue_id_t queue,
                         const struct msg_message_s* msg) {
  const int res = msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  if (res < 0) {
    if (errno == EIDRM) {
      return false;
    }
    assert(errno == 0);
  }
  return true;
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

void sigio_handler(int signum) {
  (void)signum;
  char buf[MSG_MAX_MSG_SIZE];
  if (peer_queue != -1) {
    struct msg_message_s msg = {.msg_type = MESSAGE, .msg_source = CLIENT};
    strcpy(msg.message, buf);
    msg_send_message_to(peer_queue, &msg);
  }
  scanf("%s", buf);
  if (strcmp(buf, "LIST") == 0) {
    msg_send_list_to(server_queue);
  } else if (strncmp(buf, "CONNECT", strlen("CONNECT")) == 0) {
    int peer_client_id;
    scanf("%d", &peer_client_id);
    msg_send_connect_to(server_queue, peer_client_id);
  } else if (strcmp(buf, "DISCONNECT") == 0) {
    msg_send_disconnect_to(server_queue);
  } else if (strcmp(buf, "STOP") == 0) {
    exit(0);
  }
}

void exit_handler(void) {
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

  queue = msg_init_client_queue();
  server_queue = msg_get_server_queue();
  client_id = msg_send_init_to(server_queue, queue);
  peer_queue = -1;
  printf("[Client %ld] Got id\n", client_id);
  while (true) {
    struct msg_message_s message;
    msg_receive(queue, &message);
    printf("Receved message with id %d", message.msg_type);
    switch (message.msg_type) {
      case STOP:
        exit(0);
        break;
      case CONNECT:
        peer_queue = message.connect.peer_queue;
        break;
      case DISCONNECT:
        peer_queue = -1;
        break;
      case MESSAGE:
        printf("%s\n", message.message);
      default:
        break;
    }
  }
  return 0;
}
