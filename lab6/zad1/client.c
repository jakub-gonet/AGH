#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

//   perror(strerror(errno));

msg_client_id_t client_id;
msg_queue_id_t server_queue;
msg_queue_id_t queue;

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

void msg_receive(const msg_queue_id_t from, struct msg_message_s* msg) {
  msgrcv(from, msg, MSG_STRUCT_SIZE, 0, 0);
  assert(errno == 0);
}

void msg_send_message_to(const msg_queue_id_t queue,
                         const struct msg_message_s* msg) {
  msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
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

  queue = msg_init_client_queue();
  server_queue = msg_get_server_queue();
  client_id = msg_send_init_to(server_queue, queue);
  printf("[Client %ld] Got id\n", client_id);
  while (true) {
    struct msg_message_s message;
    msg_receive(queue, &message);
    switch (message.msg_type) {
      case STOP:
        exit(0);
        break;
      default:
        break;
    }
  }
  return 0;
}
