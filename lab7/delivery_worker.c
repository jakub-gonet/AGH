#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "common.h"

static volatile sig_atomic_t keep_running = 1;

void sig_handler(int signum) {
  (void)signum;
  keep_running = 0;
}

void p_getting_pizza_from_table_msg(pid_t pid,
                                    pizza_t pizza_type,
                                    size_t pizzas_on_table) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf(
      "[%d] [%ld.%ld] Getting the pizza: %d, number of pizzas on the table: "
      "%ld\n",
      pid, s, ms, pizza_type, pizzas_on_table);
}

void p_delivering_pizza_msg(pid_t pid, pizza_t pizza_type) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Delivering the pizza: %d\n", pid, s, ms, pizza_type);
}

int main(void) {
  signal(SIGINT, sig_handler);
  pid_t self = getpid();
  srand(self);

  while (keep_running) {
    pizza_t pizza = p_get_from_table(&table);
    p_getting_pizza_from_table_msg(self, pizza,
                                   p_get_pizzas_on_table_n(&table));
    usleep(400 * 1e3);

    p_delivering_pizza_msg(self, pizza);
    usleep(400 * 1e3);
  }

  return EXIT_SUCCESS;
}
