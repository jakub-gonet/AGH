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

void p_preparing_pizza_msg(pid_t pid, int type) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Preparing pizza: %d\n", pid, s, ms, type);
}

void p_baking_pizza_msg(pid_t pid, int pizza_type, int pizzas_in_oven) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf("[%d] [%ld.%ld] Baking pizza: %d, number of pizzas in oven: %d\n", pid,
         s, ms, pizza_type, pizzas_in_oven);
}

void p_taking_pizza_msg(pid_t pid,
                        int pizza_type,
                        int pizzas_in_oven,
                        int pizzas_on_table) {
  time_t s;
  long ms;
  p_get_current_time(&s, &ms);
  printf(
      "[%d] [%ld.%ld] Taking pizza: %d, number of pizzas in oven: %d, number "
      "of "
      "pizzas on table: %d\n",
      pid, s, ms, pizza_type, pizzas_in_oven, pizzas_on_table);
}

int p_get_random_pizza(void) {
  return rand() % 10;
}

int main(void) {
  signal(SIGINT, sig_handler);
  srand(time(NULL));
  pid_t self = getpid();

  while (keep_running) {
    int pizza_type = p_get_random_pizza();
    p_preparing_pizza_msg(self, pizza_type);
    usleep(200 * 1e3);

    int pizza_index = p_place_in_oven(pizza_type);
    assert(pizza_index >= 0 && pizza_index < OVEN_SIZE);
    p_baking_pizza_msg(self, pizza_type, p_get_pizzas_in_oven_n());
    usleep(400 * 1e3);

    p_place_on_table(p_get_from_oven(pizza_index));
    p_taking_pizza_msg(self, pizza_type, p_get_pizzas_in_oven_n(),
                       p_get_pizzas_on_table_n());
  }

  return EXIT_SUCCESS;
}
