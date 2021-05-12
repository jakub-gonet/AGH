#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <unistd.h>

#include "common.h"

size_t p_to_size_t(char* str) {
  errno = 0;
  size_t val = (size_t)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

void sig_handler(int signum) {
  (void)signum;
}

void spawn_pizza_worker(void) {
  pid_t child = fork();
  assert(child >= 0);
  if (child == 0) {
    execlp("./" PIZZA_WORKER_EXEC, "./" PIZZA_WORKER_EXEC, (char*)NULL);
    assert(false);
  }
}

void spawn_delivery_worker(void) {
  pid_t child = fork();
  assert(child >= 0);
  if (child == 0) {
    execlp("./" DELIVERY_WORKER_EXEC, "./" DELIVERY_WORKER_EXEC, (char*)NULL);
    assert(false);
  }
}

int create_semaphors(void) {
  int id = semget(getpid(), pizza_sem_e_length,
                  IPC_CREAT | IPC_EXCL | SEM_ACCESS_MODE);
  // perror(strerror(errno));
  assert(id != -1);
  return id;
}

int create_shared_mem(void) {}

void remove_semaphors(void) {
  if (semaphors_id != -1) {
    semctl(semaphors_id, -1, IPC_RMID);
  }
}

void remove_shared_mem(void) {
  if (shared_mem_id != -1) {
    shmctl(shared_mem_id, IPC_RMID, NULL);
  }
}

void cleanup() {
  remove_semaphors();
  remove_shared_mem();
}

int main(int argc, char* argv[]) {
  atexit(&cleanup);
  signal(SIGINT, sig_handler);
  if (argc != 2 + 1) {
    fprintf(stderr, "Specify number of pizza workers and delivery workers\n");
    exit(EXIT_FAILURE);
  }
  p_init();

  size_t pizza_workers_n = p_to_size_t(argv[1]);
  size_t delivery_workers_n = p_to_size_t(argv[2]);

  semaphors_id = create_semaphors();
  shared_mem_id = create_shared_mem();

  printf("Hiring %ld bakers\n", pizza_workers_n);
  for (size_t i = 0; i < pizza_workers_n; ++i) {
    spawn_pizza_worker();
  }
  printf("Hiring %ld delivery workers\n", delivery_workers_n);
  for (size_t i = 0; i < delivery_workers_n; ++i) {
    spawn_delivery_worker();
  }

  while (wait(NULL) > 0 || errno == EINTR)
    ;
  printf("\nEnding...\n");
  return EXIT_SUCCESS;
}
