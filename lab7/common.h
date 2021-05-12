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

enum pizza_sem_e { OVEN, TABLE, pizza_sem_e_length };
enum access_mode_e { OBTAIN = 1, RELEASE = -1 };

struct oven_s {
  size_t next_idx;
  pizza_t pizzas[OVEN_SIZE];
};

struct table_s {
  size_t next_idx;
  pizza_t pizzas[TABLE_SIZE];
};

extern struct oven_s oven;
extern struct table_s table;
extern int semaphors_id;
extern int shared_mem_id;

void p_init(void);
void p_get_current_time(time_t* s, long* ms);
pizza_t p_get_random_pizza(void);
size_t p_place_in_oven(struct oven_s* oven, pizza_t pizza);
pizza_t p_get_from_oven(struct oven_s* oven, size_t index);
size_t p_place_on_table(struct table_s* table, pizza_t pizza);
pizza_t p_get_from_table(struct table_s* table);
size_t p_get_pizzas_in_oven_n(struct oven_s* oven);
size_t p_get_pizzas_on_table_n(struct table_s* table);

int p_get_semaphors();
void p_change_access_of(enum pizza_sem_e what, enum access_mode_e mode);

#endif  // COMMON_H
