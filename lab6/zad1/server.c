#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

struct msg_clients_list_s clients;

struct msg_client_s {
  msg_queue_id_t queue_id;
  msg_client_id_t id;
  struct msg_client_s* peer;
};

struct msg_clients_list_s {
  struct msg_client_s data[MSG_MAX_CLIENTS];
  size_t size;
};

void msg_send_message_to(const msg_queue_id_t queue,
                         const struct msg_message_s* msg) {
  msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  assert(errno == 0);
}

msg_queue_id_t msg_init_server_queue() {
  const int queue_id = msgget(MSG_SERVER_KEY, IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

msg_client_id_t msg_add_client(const msg_queue_id_t client_queue) {
  static msg_client_id_t next_client_id = 0;
  if (clients.size >= MSG_MAX_CLIENTS) {
    exit(EXIT_FAILURE);
  }
  const msg_client_id_t id = next_client_id++;
  clients.data[clients.size++] =
      (struct msg_client_s){.queue_id = client_queue, .id = id, .peer = NULL};
  return id;
}

void msg_receive(const msg_queue_id_t from, struct msg_message_s* msg) {
  do {
    msgrcv(from, msg, MSG_STRUCT_SIZE, -INIT, 0);
    assert(errno == 0);
  } while (msg->msg_source != CLIENT);
}

void msg_handle_init(struct msg_message_s* message) {
  const msg_queue_id_t client_queue = message->init.queue_id;
  const msg_client_id_t client_id = msg_add_client(client_queue);
  const struct msg_message_s msg = {
      .msg_type = INIT, .msg_source = SERVER, .init = {.client_id = client_id}};
  msg_send_message_to(client_queue, &msg);
}

int main(void) {
  const msg_queue_id_t queue = msg_init_server_queue();
  clients = (struct msg_clients_list_s){.data = {}, .size = 0};

  while (true) {
    struct msg_message_s message;
    printf("Waiting for message in %d\n", queue);
    msg_receive(queue, &message);
    switch (message.msg_type) {
      case INIT:
        msg_handle_init(&message);
        break;
      default:
        break;
    }
  }

  return 0;
}
