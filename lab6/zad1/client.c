#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

//   perror(strerror(errno));

int msg_init_client_queue() {
  printf("Creating a client with a key %d\n", getpid());
  const int queue_id = msgget(getpid(), IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

int msg_get_server_queue() {
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
    printf("type: %ld, source: %d\n", return_message.msg_type,
           return_message.msg_source);
  } while (return_message.msg_type != INIT ||
           return_message.msg_source != SERVER);
  return return_message.init.client_id;
}

int main(void) {
  int queue = msg_init_client_queue();
  int server_queue = msg_get_server_queue();
  msg_client_id_t client_id = msg_send_init_to(server_queue, queue);
  printf("[Client %d] Got id\n", client_id);
  return 0;
}
