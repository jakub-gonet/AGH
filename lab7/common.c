#include "common.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <unistd.h>

#define _POSIX_C_SOURCE 200809L

int semaphors_id = -1;
int shared_mem_id = -1;

void p_init_structs(void) {
  struct oven_s oven = {.next_idx = 0};
  struct table_s table = {.next_idx = 0};
  for (size_t i = 0; i < OVEN_SIZE; ++i) {
    oven.pizzas[i] = EMPTY_SLOT;
  }
  for (size_t i = 0; i < TABLE_SIZE; ++i) {
    table.pizzas[i] = EMPTY_SLOT;
  }
  void* shared_mem = shmat(shared_mem_id, NULL, 0);
  assert(shared_mem != (void*)-1);
  *((struct oven_s*)shared_mem) = oven;
  *((struct table_s*)(shared_mem + sizeof(struct oven_s))) = table;
}

void p_init_worker(struct oven_s** oven, struct table_s** table) {
  void* shared_mem = shmat(shared_mem_id, NULL, 0);
  assert(shared_mem != (void*)-1);
  *oven = ((struct oven_s*)shared_mem);
  *table = ((struct table_s*)(shared_mem + sizeof(struct oven_s)));
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

void p_obtain(enum pizza_sem_e what) {
  struct sembuf sem_op = {.sem_num = what, .sem_op = -1, .sem_flg = 0};
  int res = semop(semaphors_id, &sem_op, 1);
  assert(res != -1);
}

void p_release(enum pizza_sem_e what) {
  struct sembuf sem_op = {.sem_num = what, .sem_op = 1, .sem_flg = 0};
  int res = semop(semaphors_id, &sem_op, 1);
  assert(res != -1);
}

int p_get_random_pizza(void) {
  return rand() % 10;
}
int get_sem_val(enum pizza_sem_e what) {
  return semctl(semaphors_id, what, GETVAL, 0);
}

size_t p_place_in_oven(struct oven_s* oven, pizza_t pizza) {
  p_obtain(OVEN_EMPTY_SLOTS_COUNT);  // reserve one place in oven
  p_obtain(OVEN);
  size_t next_idx = oven->next_idx;
  for (size_t i = 1; oven->pizzas[next_idx] != EMPTY_SLOT; ++i) {
    assert(i <= OVEN_SIZE);
    next_idx = (next_idx + 1) % OVEN_SIZE;
  }
  oven->pizzas[next_idx] = pizza;
  oven->next_idx = (next_idx + 1) % OVEN_SIZE;
  p_release(OVEN);
  return next_idx;
}

pizza_t p_get_from_oven(struct oven_s* oven, size_t index) {
  p_obtain(OVEN);
  assert(index < OVEN_SIZE);
  pizza_t pizza = oven->pizzas[index];
  assert(pizza != EMPTY_SLOT);
  oven->pizzas[index] = EMPTY_SLOT;
  p_release(OVEN);
  p_release(OVEN_EMPTY_SLOTS_COUNT);  // free one place in oven
  return pizza;
}

size_t p_place_on_table(struct table_s* table, pizza_t pizza) {
  p_obtain(TABLE_EMPTY_SLOTS_COUNT);  // reserve one slot
  p_obtain(TABLE);
  size_t next_idx = table->next_idx;
  for (size_t i = 1; table->pizzas[next_idx] != EMPTY_SLOT; ++i) {
    assert(i <= TABLE_SIZE);
    next_idx = (next_idx + 1) % TABLE_SIZE;
  }
  table->pizzas[next_idx] = pizza;
  table->next_idx = (next_idx + 1) % TABLE_SIZE;
  p_release(TABLE);
  p_release(TABLE_FULL_SLOTS_COUNT);  // one slot filled
  return next_idx;
}

pizza_t p_get_from_table(struct table_s* table) {
  p_obtain(TABLE_FULL_SLOTS_COUNT);  // has any slot with pizza?
  p_obtain(TABLE);
  size_t next_idx = table->next_idx;
  for (size_t i = 1; table->pizzas[next_idx] == EMPTY_SLOT; ++i) {
    assert(i <= TABLE_SIZE);
    next_idx = (next_idx + 1) % TABLE_SIZE;
  }
  pizza_t pizza = table->pizzas[next_idx];
  table->pizzas[next_idx] = EMPTY_SLOT;
  assert(pizza != EMPTY_SLOT);
  p_release(TABLE);
  p_release(TABLE_EMPTY_SLOTS_COUNT);  // we took one
  return pizza;
}

size_t p_get_pizzas_in_oven_n(struct oven_s* oven) {
  size_t count = 0;
  p_obtain(OVEN);
  for (size_t i = 0; i < OVEN_SIZE; ++i) {
    if (oven->pizzas[i] != EMPTY_SLOT) {
      ++count;
    }
  }
  p_release(OVEN);
  return count;
}

size_t p_get_pizzas_on_table_n(struct table_s* table) {
  size_t count = 0;
  p_obtain(TABLE);
  for (size_t i = 0; i < TABLE_SIZE; ++i) {
    if (table->pizzas[i] != EMPTY_SLOT) {
      ++count;
    }
  }
  p_release(TABLE);
  return count;
}

int p_get_semaphors(void) {
  int id = semget(getppid(), 0, SEM_ACCESS_MODE);
  assert(id != -1);
  return id;
}

int p_get_shared_mem(void) {
  int id = shmget(getppid(), 0, SEM_ACCESS_MODE);
  assert(id != -1);
  return id;
}