#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#define ELVES_N 10
#define REINDEERS_N 9
#define ELVES_N_TRIGGER 3

enum { NO_EVENT = 0, AWAITING_REINDEERS = 1, AWAITING_ELVES = 2 } event_flags;
pthread_mutex_t rand_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t one_mutex_to_rule_them_all = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t reindeers_wakeup_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t elves_wakeup_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t santa_wakeup_cond = PTHREAD_COND_INITIALIZER;
int awaiting_reindeers_count = 0;
struct {
  size_t size;
  size_t elves_with_problems[ELVES_N_TRIGGER];
} awaiting_elves;

#define with(mutex)                                              \
  for (bool _x = true; _x && (pthread_mutex_lock(&mutex), true); \
       _x = (pthread_mutex_unlock(&mutex), false))

void add_to_problems_list(size_t elf_id) {
  if (awaiting_elves.size == ELVES_N_TRIGGER) {
    printf("[Elf %lu] Waking up Santa.\n", elf_id);
    event_flags |= AWAITING_ELVES;
    pthread_cond_signal(&santa_wakeup_cond);
    while (awaiting_elves.size == ELVES_N_TRIGGER) {
      pthread_cond_wait(&elves_wakeup_cond, &one_mutex_to_rule_them_all);
    }
  }
  awaiting_elves.elves_with_problems[awaiting_elves.size++] = elf_id;
  printf("[Elf %lu] %lu elves waiting for santa.\n", elf_id,
         awaiting_elves.size);
}

void clear_problems_list(void) {
  awaiting_elves.size = 0;
}

void wait_rand_us(unsigned lower_us, unsigned upper_us) {
  int time;
  with(rand_mutex) { time = (rand() % (upper_us - lower_us + 1)) + lower_us; }
  usleep(time * 1e3);
}

void* elf(void* args) {
  size_t id = (size_t)args;
  while (true) {
    wait_rand_us(200, 500);
    with(one_mutex_to_rule_them_all) { add_to_problems_list(id); }
  }
  return NULL;
}

void* reindeer(void* args) {
  size_t id = (size_t)args;
  while (true) {
    wait_rand_us(500, 10000);
    with(one_mutex_to_rule_them_all) {
      while (awaiting_reindeers_count == REINDEERS_N) {
        pthread_cond_wait(&reindeers_wakeup_cond, &one_mutex_to_rule_them_all);
      }

      awaiting_reindeers_count++;

      printf("[Reindeer %lu] %d reindeers waiting for Santa.\n", id,
             awaiting_reindeers_count);
      if (awaiting_reindeers_count == REINDEERS_N) {
        printf("[Reindeer %lu] Waking up Santa.\n", id);
        event_flags |= AWAITING_REINDEERS;
        pthread_cond_signal(&santa_wakeup_cond);
      }
    }
  }
  return NULL;
}

bool handle_events(int* n_delivered) {
  if (event_flags == NO_EVENT) {
    return false;
  }
  printf("[Santa] Waking up.\n");

  if (event_flags & AWAITING_REINDEERS) {
    printf("[Santa] Delivering toys.\n");
    wait_rand_us(200, 400);
    awaiting_reindeers_count = 0;
    (*n_delivered)--;
    event_flags ^= AWAITING_REINDEERS;
    pthread_cond_broadcast(&reindeers_wakeup_cond);
  }
  if (event_flags & AWAITING_ELVES) {
    printf("[Santa] Solving elves' problems. (");
    for (size_t i = 0; i < awaiting_elves.size; ++i) {
      bool is_last = i == awaiting_elves.size - 1;
      printf(is_last ? "%ld)\n" : "%ld, ",
             awaiting_elves.elves_with_problems[i]);
    }
    awaiting_elves.size = 0;
    event_flags ^= AWAITING_ELVES;
    pthread_cond_broadcast(&elves_wakeup_cond);
  }
  printf("[Santa] Going to sleep.\n");

  return true;
}

void* santa(void* args) {
  (void)args;
  int n_delivered = 3;
  with(one_mutex_to_rule_them_all) {
    while (n_delivered > 0) {
      if (!handle_events(&n_delivered)) {
        pthread_cond_wait(&santa_wakeup_cond, &one_mutex_to_rule_them_all);
      }
    }
  }
  exit(EXIT_SUCCESS);
}

int main(void) {
  srand(time(NULL));
  pthread_t santa_thr;
  pthread_t reindeers_thr[REINDEERS_N];
  pthread_t elves_thr[ELVES_N];
  awaiting_elves.size = 0;

  for (size_t i = 0; i < REINDEERS_N; i++) {
    pthread_create(&reindeers_thr[i], NULL, &reindeer, (void*)i);
  }
  for (size_t i = 0; i < ELVES_N; i++) {
    pthread_create(&elves_thr[i], NULL, &elf, (void*)i);
  }
  pthread_create(&santa_thr, NULL, &santa, NULL);
  pthread_join(santa_thr, NULL);
  return 0;
}
