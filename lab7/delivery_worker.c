#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "common.h"

static volatile sig_atomic_t keep_running = 1;

void sig_handler(int signum) {
  (void)signum;
  keep_running = 0;
}

void p_getting_pizza_from_table_msg(pid_t pid,
                                    int pizza_type,
                                    int pizzas_on_table) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Getting pizza: %d, number of pizzas on table: %d\n",
         pid, s, ms, pizza_type, pizzas_on_table);
}

void p_delivering_pizza_msg(pid_t pid, int pizza_type) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Delivering pizza: %d\n", pid, s, ms, pizza_type);
}

int main(void) {
  signal(SIGINT, sig_handler);
  while (keep_running) {
  }

  return EXIT_SUCCESS;
}
