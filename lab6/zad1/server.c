#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

struct msg_clients_list_s clients;
msg_queue_id_t queue;

struct msg_client_s {
  msg_queue_id_t queue_id;
  msg_client_id_t id;
  struct msg_client_s* peer;
};

struct msg_clients_list_s {
  struct msg_client_s data[MSG_MAX_CLIENTS];
  size_t last_idx;
  size_t size;
};

void msg_send_message_to(const msg_queue_id_t queue,
                         const struct msg_message_s* msg) {
  msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  assert(errno == 0);
}

msg_queue_id_t msg_init_server_queue(void) {
  const int queue_id = msgget(MSG_SERVER_KEY, IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

msg_client_id_t msg_add_client(const msg_queue_id_t client_queue) {
  static msg_client_id_t next_client_id = 0;
  if (clients.size + 1 >= MSG_MAX_CLIENTS) {
    exit(EXIT_FAILURE);
  }
  const msg_client_id_t id = next_client_id++;
  clients.data[++clients.last_idx] =
      (struct msg_client_s){.queue_id = client_queue, .id = id, .peer = NULL};
  ++clients.size;
  return id;
}

ssize_t msg_find_client(const msg_client_id_t client_id) {
  for (size_t i = 0; i < clients.size; ++i) {
    if (clients.data[i].id == client_id) {
      return i;
    }
  }
  return -1;
}

bool msg_is_removed(const msg_client_id_t client_id) {
  return msg_find_client(client_id) == -1;
}

bool msg_remove_client(const msg_client_id_t client_id) {
  const ssize_t idx = msg_find_client(client_id);
  if (idx != -1) {
    clients.data[idx].id = -1;
    --clients.size;
    msgctl(clients.data[idx].queue_id, IPC_RMID, NULL);
    return true;
  }
  return false;
}

void msg_receive(const msg_queue_id_t from, struct msg_message_s* msg) {
  msgrcv(from, msg, MSG_STRUCT_SIZE, -INIT, 0);
  assert(errno == 0);
  assert(msg->msg_source == CLIENT);
}

void msg_receive_of_type(const msg_queue_id_t from,
                         struct msg_message_s* msg,
                         const enum msg_server_request_t type) {
  msgrcv(from, msg, MSG_STRUCT_SIZE, type, 0);
  assert(errno == 0);
  assert(msg->msg_source == CLIENT);
}

void msg_handle_init(struct msg_message_s* message) {
  const msg_queue_id_t client_queue = message->init.queue_id;
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

void msg_stop_all() {
  const struct msg_message_s msg = {
      .msg_type = STOP,
      .msg_source = SERVER,
  };
  struct msg_message_s received;

  for (size_t i = 0; i <= clients.last_idx; i++) {
    const struct msg_client_s client = clients.data[i];
    if (msg_is_removed(client.id)) {
      continue;
    }
    msg_send_message_to(client.queue_id, &msg);
    msg_receive_of_type(queue, &received, STOP);
    msg_handle_stop(&received);
  }
}

void exit_handler(void) {
  msg_stop_all();
  msgctl(queue, IPC_RMID, NULL);
}

void sigint_handler(int signum) {
  (void)signum;
  exit(0);
}

int main(void) {
  atexit(exit_handler);
  signal(SIGINT, sigint_handler);

  queue = msg_init_server_queue();
  clients = (struct msg_clients_list_s){.data = {}, .size = 0, .last_idx = -1};

  while (true) {
    struct msg_message_s message;
    msg_receive(queue, &message);
    printf("Got message of type %ld\n", message.msg_type);
    switch (message.msg_type) {
      case INIT:
        msg_handle_init(&message);
        break;
      case STOP:
        msg_handle_stop(&message);
        break;
      default:
        break;
    }
  }

  return 0;
}
