#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <unistd.h>
#include "common.h"

//   perror(strerror(errno));

int init_client_queue() {
  printf("Creating a client with a key %d\n", getpid());
  const int queue_id = msgget(getpid(), IPC_CREAT | IPC_EXCL | 0666);
  assert(queue_id != -1);
  return queue_id;
}

int get_server_queue() {
  const int queue_id = msgget(SERVER_KEY, 0);
  assert(queue_id != -1);
  return queue_id;
}

void receive(const queue_id_t from, struct message_t* msg) {
  msgrcv(from, msg, MSG_STRUCT_SIZE, 0, 0);
  assert(errno == 0);
}

void send_message_to(const queue_id_t queue, const struct message_t* msg) {
  msgsnd(queue, msg, MSG_STRUCT_SIZE, 0);
  assert(errno == 0);
}

client_id_t send_init_to(const queue_id_t destination,
                         const queue_id_t own_queue) {
  const struct message_t msg = {
      .msg_type = INIT,
      .msg_source = CLIENT,
      .payload = {(struct init_t){.queue_id = own_queue}}};
  send_message_to(destination, &msg);
  struct message_t return_message;
  do {
    receive(own_queue, &return_message);
    printf("type: %ld, source: %d\n", return_message.msg_type,
           return_message.msg_source);
  } while (return_message.msg_type != INIT ||
           return_message.msg_source != SERVER);
  return return_message.payload.init.client_id;
}

int main(void) {
  int queue = init_client_queue();
  int server_queue = get_server_queue();
  client_id_t client_id = send_init_to(server_queue, queue);
  printf("[Client %d] Got id\n", client_id);
  return 0;
}
