#if !defined(COMMON_H)
#define COMMON_H
#include <time.h>

#define OVEN_SIZE 5
#define TABLE_SIZE 5
#define EMPTY_SLOT 0xaa
#define PIZZA_WORKER_EXEC "pizza_worker"
#define DELIVERY_WORKER_EXEC "delivery_worker"

#define SEM_ACCESS_MODE 0666

typedef int pizza_t;

enum pizza_sem_e {
  OVEN,
  TABLE,
  OVEN_EMPTY_SLOTS_COUNT,
  TABLE_FULL_SLOTS_COUNT,
  TABLE_EMPTY_SLOTS_COUNT,
  pizza_sem_e_length
};

struct oven_s {
  size_t next_idx;
  pizza_t pizzas[OVEN_SIZE];
};

struct table_s {
  size_t next_idx;
  pizza_t pizzas[TABLE_SIZE];
};

extern int semaphors_id;
extern int shared_mem_id;

void p_init_structs(void);
void p_init_worker();
void p_get_current_time(time_t* s, long* ms);
pizza_t p_get_random_pizza(void);
size_t p_place_in_oven(struct oven_s* oven, pizza_t pizza);
pizza_t p_get_from_oven(struct oven_s* oven, size_t index);
size_t p_place_on_table(struct table_s* table, pizza_t pizza);
pizza_t p_get_from_table(struct table_s* table);
size_t p_get_pizzas_in_oven_n(struct oven_s* oven);
size_t p_get_pizzas_on_table_n(struct table_s* table);

int p_get_semaphors(void);
int p_get_shared_mem(void);

#endif  // COMMON_H
