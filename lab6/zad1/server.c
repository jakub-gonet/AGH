#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

struct clients_list_t clients;

struct client_t {
  queue_id_t queue_id;
  client_id_t id;
  struct client_t* peer;
};

struct clients_list_t {
  struct client_t data[MAX_CLIENTS];
  size_t size;
};

void send_message_to(const queue_id_t queue, const struct message_t* msg) {
  msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  assert(errno == 0);
}

queue_id_t init_server_queue() {
  const int queue_id = msgget(SERVER_KEY, IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

client_id_t add_client(const queue_id_t client_queue) {
  static client_id_t next_client_id = 0;
  if (clients.size >= MAX_CLIENTS) {
    exit(EXIT_FAILURE);
  }
  const client_id_t id = next_client_id++;
  clients.data[clients.size++] =
      (struct client_t){.queue_id = client_queue, .id = id, .peer = NULL};
  return id;
}

void receive(const queue_id_t from, struct message_t* msg) {
  do {
    msgrcv(from, msg, MSG_STRUCT_SIZE, -INIT, 0);
    assert(errno == 0);
  } while (msg->msg_source != CLIENT);
}

void handle_init(struct message_t* message) {
  const queue_id_t client_queue = message->payload.init.queue_id;
  const client_id_t client_id = add_client(client_queue);
  const struct message_t msg = {.msg_type = INIT,
                                .msg_source = SERVER,
                                .payload = {{.client_id = client_id}}};
  send_message_to(client_queue, &msg);
}

int main(void) {
  const queue_id_t queue = init_server_queue();
  clients = (struct clients_list_t){.data = {}, .size = 0};

  while (true) {
    struct message_t message;
    printf("Waiting for message in %d\n", queue);
    receive(queue, &message);
    switch (message.msg_type) {
      case INIT:
        handle_init(&message);
        break;
      default:
        break;
    }
  }

  return 0;
}
