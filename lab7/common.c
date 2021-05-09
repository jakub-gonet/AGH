#include "common.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>

#define _POSIX_C_SOURCE 200809L

struct oven_s oven = (struct oven_s){.size = 0, .next_idx = 0};
struct table_s table = (struct table_s){.size = 0, .next_idx = 0};

void p_init(void) {
  srand(time(NULL));
  for (size_t i = 0; i < OVEN_SIZE; ++i) {
    oven.pizzas[i] = EMPTY_SLOT;
  }
  for (size_t i = 0; i < TABLE_SIZE; ++i) {
    table.pizzas[i] = EMPTY_SLOT;
  }
}

void p_get_current_time(time_t* s, long* ms) {
  struct timespec spec;
  clock_gettime(CLOCK_REALTIME, &spec);
  *s = spec.tv_sec;
  *ms = round(spec.tv_nsec / 1.0e6);
  if (*ms > 999) {
    (*s)++;
    (*ms) = 0;
  }
}

int p_get_random_pizza(void) {
  return rand() % 10;
}

size_t p_place_in_oven(struct oven_s* oven, pizza_t pizza) {
  size_t next_idx = oven->next_idx;
  for (size_t i = 1; oven->pizzas[next_idx] != EMPTY_SLOT; ++i) {
    assert(i <= OVEN_SIZE);
    next_idx = (next_idx + 1) % OVEN_SIZE;
  }
  oven->pizzas[next_idx] = pizza;
  ++oven->size;
  oven->next_idx = (next_idx + 1) % OVEN_SIZE;
  return next_idx;
}

pizza_t p_get_from_oven(struct oven_s* oven, size_t index) {
  assert(index < OVEN_SIZE);
  --oven->size;
  pizza_t pizza = oven->pizzas[index];
  assert(pizza != EMPTY_SLOT);
  oven->pizzas[index] = EMPTY_SLOT;
  return pizza;
}

size_t p_place_on_table(struct table_s* table, pizza_t pizza) {
  size_t next_idx = table->next_idx;
  for (size_t i = 1; table->pizzas[next_idx] != EMPTY_SLOT; ++i) {
    assert(i <= TABLE_SIZE);
    next_idx = (next_idx + 1) % TABLE_SIZE;
  }
  table->pizzas[next_idx] = pizza;
  ++table->size;
  table->next_idx = (next_idx + 1) % TABLE_SIZE;
  return next_idx;
}

pizza_t p_get_from_table(struct table_s* table) {
  size_t next_idx = table->next_idx;
  for (size_t i = 1; table->pizzas[next_idx] == EMPTY_SLOT; ++i) {
    assert(i <= TABLE_SIZE);
    next_idx = (next_idx + 1) % TABLE_SIZE;
  }
  --table->size;
  pizza_t pizza = table->pizzas[next_idx];
  assert(pizza != EMPTY_SLOT);
  return pizza;
}

size_t p_get_pizzas_in_oven_n(struct oven_s* oven) {
  return oven->size;
}

size_t p_get_pizzas_on_table_n(struct table_s* table) {
  return table->size;
}