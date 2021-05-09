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

  while (keep_running) {
  }

  return EXIT_SUCCESS;
}
