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

void p_preparing_pizza_msg(pid_t pid, pizza_t type) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Preparing a pizza: %d\n", pid, s, ms, type);
}

void p_baking_pizza_msg(pid_t pid, pizza_t pizza_type, size_t pizzas_in_oven) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf(
      "[%d] [%ld.%ld] Baking the pizza: %d, number of pizzas in the oven: "
      "%ld\n",
      pid, s, ms, pizza_type, pizzas_in_oven);
}

void p_taking_pizza_msg(pid_t pid,
                        pizza_t pizza_type,
                        size_t pizzas_in_oven,
                        size_t pizzas_on_table) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf(
      "[%d] [%ld.%ld] Taking the pizza: %d, number of pizzas in the oven: %ld, "
      "number of pizzas on the table: %ld\n",
      pid, s, ms, pizza_type, pizzas_in_oven, pizzas_on_table);
}

int main(void) {
  signal(SIGINT, sig_handler);
  pid_t self = getpid();
  srand(self);

  semaphors_id = p_get_semaphors();
  shared_mem_id = p_get_shared_mem();

  struct oven_s* oven;
  struct table_s* table;
  p_init_worker(&oven, &table);

  while (keep_running) {
    pizza_t pizza = p_get_random_pizza();
    p_preparing_pizza_msg(self, pizza);
    usleep(200 * 1e3);

    size_t pizza_index = p_place_in_oven(oven, pizza);
    p_baking_pizza_msg(self, pizza, p_get_pizzas_in_oven_n(oven));
    usleep(400 * 1e3);

    p_place_on_table(table, p_get_from_oven(oven, pizza_index));
    p_taking_pizza_msg(self, pizza, p_get_pizzas_in_oven_n(oven),
                       p_get_pizzas_on_table_n(table));
  }

  return EXIT_SUCCESS;
}