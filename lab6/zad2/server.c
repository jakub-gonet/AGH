#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <mqueue.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "common.h"

struct msg_clients_list_s clients;
mqd_t queue;

struct msg_client_s {
  mqd_t queue_id;
  msg_client_id_t id;
  struct msg_client_s* peer;
};

struct msg_clients_list_s {
  struct msg_client_s data[MSG_MAX_CLIENTS];
  size_t size;
};

void msg_send_message_to(const mqd_t queue, const struct msg_message_s* msg) {
  char msg_serialized[MSG_MAX_REQUEST_SIZE];
  memcpy(msg_serialized, msg, sizeof(*msg));
  mq_send(queue, msg_serialized, sizeof(msg), msg->msg_type);
  assert(errno == 0);
}

mqd_t msg_init_server_queue(void) {
  const mqd_t queue_id =
      mq_open(MSG_SERVER_PATH, O_RDONLY | O_CREAT | O_EXCL, 0666, NULL);
  assert(queue_id != -1);
  return queue_id;
}

msg_client_id_t msg_add_client(const mqd_t client_queue) {
  static msg_client_id_t next_client_id = 0;
  if (clients.size + 1 >= MSG_MAX_CLIENTS) {
    exit(EXIT_FAILURE);
  }
  const msg_client_id_t id = next_client_id++;
  clients.data[clients.size++] =
      (struct msg_client_s){.queue_id = client_queue, .id = id, .peer = NULL};
  return id;
}

struct msg_client_s* msg_find_client(const msg_client_id_t client_id) {
  for (size_t i = 0; i < clients.size; ++i) {
    const struct msg_client_s client = clients.data[i];
    if (client.id != -1 && client.id == client_id) {
      return &clients.data[i];
    }
  }
  return NULL;
}

bool msg_is_removed(const msg_client_id_t client_id) {
  return msg_find_client(client_id) == NULL;
}

bool msg_is_connected(const msg_client_id_t client_id) {
  const struct msg_client_s* client = msg_find_client(client_id);
  if (client == NULL || client->peer == NULL) {
    return false;
  }
  return true;
}

bool msg_remove_client(const msg_client_id_t client_id) {
  struct msg_client_s* client = msg_find_client(client_id);
  if (client != NULL) {
    client->id = -1;
    const int res = mq_close(client->queue_id);
    assert(res != -1);
    return true;
  }
  return false;
}

void msg_receive(const mqd_t from, struct msg_message_s* msg) {
  char msg_serialized[MSG_MAX_REQUEST_SIZE];
  mq_receive(from, msg_serialized, sizeof(msg), NULL);
  memcpy(msg, msg_serialized, sizeof(*msg));
  assert(errno == 0);
  assert(msg->msg_source == CLIENT);
}

void msg_receive_of_type(const mqd_t from,
                         struct msg_message_s* msg,
                         const enum msg_server_request_t type) {
  do {
    msg_receive(from, msg);
  } while (msg->msg_type != type);
}

void msg_handle_init(struct msg_message_s* message) {
  const mqd_t client_queue = mq_open(message->init.queue_name, O_WRONLY);
  assert(client_queue != -1);
  const msg_client_id_t client_id = msg_add_client(client_queue);
  const struct msg_message_s msg = {
      .msg_type = INIT, .msg_source = SERVER, .init = {.client_id = client_id}};
  msg_send_message_to(client_queue, &msg);
}

void msg_handle_stop(struct msg_message_s* message) {
  const msg_client_id_t id_to_remove = message->stop.client_id;
  const bool is_removed = msg_remove_client(id_to_remove);
  assert(is_removed);
}

void msg_handle_list() {
  printf("=== Connected clients ===\n");
  for (size_t i = 0; i < clients.size; i++) {
    const struct msg_client_s client = clients.data[i];
    if (msg_is_removed(client.id)) {
      continue;
    }
    printf("Client id: %ld, queue id: %d, available: %s\n", client.id,
           client.queue_id, msg_is_connected(client.id) ? "no" : "yes");
  }
}

void msg_handle_connect(struct msg_message_s* message) {
  const msg_client_id_t source_id = message->connect.client_id;
  const msg_client_id_t target_id = message->connect.id_to_connect;
  if (msg_is_connected(source_id) || msg_is_connected(target_id)) {
    return;
  }
  struct msg_client_s* source = msg_find_client(source_id);
  struct msg_client_s* target = msg_find_client(target_id);
  source->peer = target;
  target->peer = source;
  struct msg_message_s msg = {.msg_type = CONNECT,
                              .msg_source = SERVER,
                              .connect = {.peer_queue = source->queue_id}};
  msg_send_message_to(target->queue_id, &msg);
  msg.connect.peer_queue = target->queue_id;
  msg_send_message_to(source->queue_id, &msg);
}

void msg_handle_disconnect(struct msg_message_s* message) {
  struct msg_client_s* client = msg_find_client(message->disconnect.client_id);
  struct msg_message_s msg = {
      .msg_type = DISCONNECT,
      .msg_source = SERVER,
  };
  msg_send_message_to(client->queue_id, &msg);
  msg_send_message_to(client->peer->queue_id, &msg);
  client->peer->peer = NULL;
  client->peer = NULL;
}

void msg_stop_all() {
  for (size_t i = 0; i < clients.size; i++) {
    const struct msg_client_s client = clients.data[i];
    if (msg_is_removed(client.id)) {
      continue;
    }

    struct msg_message_s msg = {.disconnect.client_id = client.id};
    if (client.peer != NULL) {
      msg_handle_disconnect(&msg);
    }
    msg = (struct msg_message_s){
        .msg_type = STOP,
        .msg_source = SERVER,
    };
    struct msg_message_s received;
    msg_send_message_to(client.queue_id, &msg);
    msg_receive_of_type(queue, &received, STOP);
    msg_handle_stop(&received);
  }
}

void exit_handler(void) {
  msg_stop_all();
  mq_unlink(MSG_SERVER_PATH);
}

void sigint_handler(int signum) {
  (void)signum;
  exit(0);
}

int main(void) {
  if (MQ_PRIO_MAX <= LAST) {
    exit(1);
  }
  atexit(exit_handler);
  signal(SIGINT, sigint_handler);
  queue = msg_init_server_queue();
  clients = (struct msg_clients_list_s){.data = {}, .size = 0};

  while (true) {
    struct msg_message_s message;
    msg_receive(queue, &message);
    // printf("Got message of type %ld\n", message.msg_type);
    switch (message.msg_type) {
      case INIT:
        msg_handle_init(&message);
        break;
      case STOP:
        msg_handle_stop(&message);
        break;
      case LIST:
        msg_handle_list();
        break;
      case CONNECT:
        msg_handle_connect(&message);
        break;
      case DISCONNECT:
        msg_handle_disconnect(&message);
        break;
      default:
        exit(1);
    }
  }

  return 0;
}
