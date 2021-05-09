#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
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
  }
}

void spawn_delivery_worker(void) {
  pid_t child = fork();
  assert(child >= 0);
  if (child == 0) {
    execlp("./" DELIVERY_WORKER_EXEC, "./" DELIVERY_WORKER_EXEC, (char*)NULL);
  }
}

int main(int argc, char* argv[]) {
  signal(SIGINT, sig_handler);
  if (argc != 2 + 1) {
    fprintf(stderr, "Specify number of pizza workers and delivery workers\n");
    exit(EXIT_FAILURE);
  }
  p_init();

  size_t pizza_workers_n = p_to_size_t(argv[1]);
  size_t delivery_workers_n = p_to_size_t(argv[2]);
  printf("Hiring %ld bakers\n", pizza_workers_n);
  for (size_t i = 0; i < pizza_workers_n; ++i) {
    spawn_pizza_worker();
  }
  printf("Hiring %ld delivery workers\n", delivery_workers_n);
  for (size_t i = 0; i < delivery_workers_n; ++i) {
    spawn_delivery_worker();
  }

  pause();
  printf("\nEnding...\n");
  return EXIT_SUCCESS;
}
