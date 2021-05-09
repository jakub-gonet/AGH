#if !defined(COMMON_H)
#define COMMON_H
#include <time.h>

#define OVEN_SIZE 5
#define TABLE_SIZE 5
#define EMPTY_SLOT 0xaa
#define PIZZA_WORKER_EXEC "pizza_worker"
#define DELIVERY_WORKER_EXEC "delivery_worker"

typedef int pizza_t;

struct oven_s {
  size_t size;
  size_t next_idx;
  pizza_t pizzas[OVEN_SIZE];
};

struct table_s {
  size_t size;
  size_t next_idx;
  pizza_t pizzas[TABLE_SIZE];
};

extern struct oven_s oven;
extern struct table_s table;

void p_init(void);
void p_get_current_time(time_t* s, long* ms);
pizza_t p_get_random_pizza(void);
size_t p_place_in_oven(struct oven_s* oven, pizza_t pizza);
pizza_t p_get_from_oven(struct oven_s* oven, size_t index);
size_t p_place_on_table(struct table_s* table, pizza_t pizza);
pizza_t p_get_from_table(struct table_s* table);
size_t p_get_pizzas_in_oven_n(struct oven_s* oven);
size_t p_get_pizzas_on_table_n(struct table_s* table);

#endif  // COMMON_H
