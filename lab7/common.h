#if !defined(COMMON_H)
#define COMMON_H
#include <time.h>

#define OVEN_SIZE 5
#define TABLE_SIZE 5

typedef int pizza_t;

void p_get_current_time(time_t* s, long* ms);

#endif  // COMMON_H
